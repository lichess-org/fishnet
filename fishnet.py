#!/usr/bin/env python
# -*- coding: utf-8 -*-
# PYTHON_ARGCOMPLETE_OK

# This file is part of the lichess.org fishnet client.
# Copyright (c) 2016-2020 Niklas Fiekas <niklas.fiekas@backscattering.de>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Distributed Stockfish analysis for lichess.org"""

from __future__ import print_function
from __future__ import division

import argparse
import logging
import json
import time
import random
import collections
import contextlib
import multiprocessing
import threading
import site
import struct
import sys
import os
import stat
import platform
import re
import textwrap
import getpass
import signal
import ctypes
import string

try:
    import requests
except ImportError:
    print("fishnet requires the 'requests' module.", file=sys.stderr)
    print("Try 'pip install requests' or install python-requests from your distro packages.", file=sys.stderr)
    print(file=sys.stderr)
    raise

if os.name == "posix" and sys.version_info[0] < 3:
    try:
        import subprocess32 as subprocess
    except ImportError:
        import subprocess
else:
    import subprocess

try:
    import urlparse
except ImportError:
    import urllib.parse as urlparse

try:
    import configparser
except ImportError:
    import ConfigParser as configparser

try:
    import queue
except ImportError:
    import Queue as queue

try:
    from shlex import quote as shell_quote
except ImportError:
    from pipes import quote as shell_quote

try:
    # Python 2
    input = raw_input
except NameError:
    pass

try:
    # Python 3
    DEAD_ENGINE_ERRORS = (EOFError, IOError, BrokenPipeError)
except NameError:
    # Python 2
    DEAD_ENGINE_ERRORS = (EOFError, IOError)


__version__ = "1.17.2"  # remember to update changelog

__author__ = "Niklas Fiekas"
__email__ = "niklas.fiekas@backscattering.de"
__license__ = "GPLv3+"

DEFAULT_CONFIG = "fishnet.ini"
DEFAULT_ENDPOINT = "https://lichess.org/fishnet/"
STOCKFISH_RELEASES = "https://api.github.com/repos/niklasf/Stockfish/releases/latest"
DEFAULT_THREADS = 3
HASH_MIN = 16
HASH_DEFAULT = 256
HASH_MAX = 512
MAX_BACKOFF = 30.0
MAX_FIXED_BACKOFF = 3.0
TARGET_MOVE_TIME = 2.0
MAX_MOVE_TIME = 6.0
MAX_SLOW_BACKOFF = (MAX_MOVE_TIME - TARGET_MOVE_TIME) * 60  # time diff * plies
HTTP_TIMEOUT = 15.0
STAT_INTERVAL = 60.0
PROGRESS_REPORT_INTERVAL = 5.0
CHECK_PYPI_CHANCE = 0.01
LVL_SKILL = [0, 3, 6, 10, 14, 16, 18, 20]
LVL_MOVETIMES = [50, 100, 150, 200, 300, 400, 500, 1000]
LVL_DEPTHS = [1, 1, 2, 3, 5, 8, 13, 22]


def intro():
    return r"""
.   _________         .    .
.  (..       \_    ,  |\  /|
.   \       O  \  /|  \ \/ /
.    \______    \/ |   \  /      _____ _     _     _   _      _
.       vvvv\    \ |   /  |     |  ___(_)___| |__ | \ | | ___| |_
.       \^^^^  ==   \_/   |     | |_  | / __| '_ \|  \| |/ _ \ __|
.        `\_   ===    \.  |     |  _| | \__ \ | | | |\  |  __/ |_
.        / /\_   \ /      |     |_|   |_|___/_| |_|_| \_|\___|\__| %s
.        |/   \_  \|      /
.               \________/      Distributed Stockfish analysis for lichess.org
""".lstrip() % __version__


PROGRESS = 15
ENGINE = 5
logging.addLevelName(PROGRESS, "PROGRESS")
logging.addLevelName(ENGINE, "ENGINE")


class LogFormatter(logging.Formatter):
    def format(self, record):
        # Format message
        msg = super(LogFormatter, self).format(record)

        # Add level name
        if record.levelno in [logging.INFO, PROGRESS]:
            with_level = msg
        else:
            with_level = "%s: %s" % (record.levelname, msg)

        # Add thread name
        if record.threadName == "MainThread":
            return with_level
        else:
            return "%s: %s" % (record.threadName, with_level)


class CollapsingLogHandler(logging.StreamHandler):
    def __init__(self, stream=sys.stdout):
        super(CollapsingLogHandler, self).__init__(stream)
        self.last_level = logging.INFO
        self.last_len = 0

    def emit(self, record):
        try:
            if self.last_level == PROGRESS:
                if record.levelno == PROGRESS:
                    self.stream.write("\r")
                else:
                    self.stream.write("\n")

            msg = self.format(record)
            if record.levelno == PROGRESS:
                self.stream.write(msg.ljust(self.last_len))
                self.last_len = max(len(msg), self.last_len)
            else:
                self.last_len = 0
                self.stream.write(msg)
                self.stream.write("\n")

            self.last_level = record.levelno
            self.flush()
        except Exception:
            self.handleError(record)


class TailLogHandler(logging.Handler):
    def __init__(self, capacity, max_level, flush_level, target_handler):
        super(TailLogHandler, self).__init__()
        self.buffer = collections.deque(maxlen=capacity)
        self.max_level = max_level
        self.flush_level = flush_level
        self.target_handler = target_handler

    def emit(self, record):
        if record.levelno < self.max_level:
            self.buffer.append(record)

        if record.levelno >= self.flush_level:
            while self.buffer:
                record = self.buffer.popleft()
                self.target_handler.handle(record)


class CensorLogFilter(logging.Filter):
    def __init__(self, keyword):
        self.keyword = keyword

    def censor(self, msg):
        try:
            # Python 2
            if not isinstance(msg, basestring):
                return msg
        except NameError:
            # Python 3
            if not isinstance(msg, str):
                return msg

        if self.keyword:
            return msg.replace(self.keyword, "*" * len(self.keyword))
        else:
            return msg

    def filter(self, record):
        record.msg = self.censor(record.msg)
        record.args = tuple(self.censor(arg) for arg in record.args)
        return True


def setup_logging(verbosity, stream=sys.stdout):
    logger = logging.getLogger()
    logger.setLevel(ENGINE)

    handler = logging.StreamHandler(stream)

    if verbosity >= 3:
        handler.setLevel(ENGINE)
    elif verbosity >= 2:
        handler.setLevel(logging.DEBUG)
    elif verbosity >= 1:
        handler.setLevel(PROGRESS)
    else:
        if stream.isatty():
            handler = CollapsingLogHandler(stream)
            handler.setLevel(PROGRESS)
        else:
            handler.setLevel(logging.INFO)

    if verbosity < 2:
        logging.getLogger("urllib3").setLevel(logging.WARNING)
        logging.getLogger("requests.packages.urllib3").setLevel(logging.WARNING)

    tail_target = logging.StreamHandler(stream)
    tail_target.setFormatter(LogFormatter())
    logger.addHandler(TailLogHandler(35, handler.level, logging.ERROR, tail_target))

    handler.setFormatter(LogFormatter())
    logger.addHandler(handler)


def base_url(url):
    url_info = urlparse.urlparse(url)
    return "%s://%s/" % (url_info.scheme, url_info.hostname)


class ConfigError(Exception):
    pass


class UpdateRequired(Exception):
    pass


class Shutdown(Exception):
    pass


class ShutdownSoon(Exception):
    pass


class SignalHandler(object):
    def __init__(self):
        self.ignore = False

        signal.signal(signal.SIGTERM, self.handle_term)
        signal.signal(signal.SIGINT, self.handle_int)

        try:
            signal.signal(signal.SIGUSR1, self.handle_usr1)
        except AttributeError:
            # No SIGUSR1 on Windows
            pass

    def handle_int(self, signum, frame):
        if not self.ignore:
            self.ignore = True
            raise ShutdownSoon()

    def handle_term(self, signum, frame):
        if not self.ignore:
            self.ignore = True
            raise Shutdown()

    def handle_usr1(self, signum, frame):
        if not self.ignore:
            self.ignore = True
            raise UpdateRequired()


def open_process(command, cwd=None, shell=True, _popen_lock=threading.Lock()):
    kwargs = {
        "shell": shell,
        "stdout": subprocess.PIPE,
        "stderr": subprocess.STDOUT,
        "stdin": subprocess.PIPE,
        "bufsize": 1,  # Line buffered
        "universal_newlines": True,
    }

    if cwd is not None:
        kwargs["cwd"] = cwd

    # Prevent signal propagation from parent process
    try:
        # Windows
        kwargs["creationflags"] = subprocess.CREATE_NEW_PROCESS_GROUP
    except AttributeError:
        # Unix
        kwargs["preexec_fn"] = os.setpgrp

    with _popen_lock:  # Work around Python 2 Popen race condition
        return subprocess.Popen(command, **kwargs)


def kill_process(p):
    try:
        # Windows
        p.send_signal(signal.CTRL_BREAK_EVENT)
    except AttributeError:
        # Unix
        os.killpg(p.pid, signal.SIGKILL)

    p.communicate()


def send(p, line):
    logging.log(ENGINE, "%s << %s", p.pid, line)
    p.stdin.write(line + "\n")
    p.stdin.flush()


def recv(p):
    while True:
        line = p.stdout.readline()
        if line == "":
            raise EOFError()

        line = line.rstrip()

        logging.log(ENGINE, "%s >> %s", p.pid, line)

        if line:
            return line


def recv_uci(p):
    command_and_args = recv(p).split(None, 1)
    if len(command_and_args) == 1:
        return command_and_args[0], ""
    elif len(command_and_args) == 2:
        return command_and_args


def uci(p):
    send(p, "uci")

    engine_info = {}
    variants = set()

    while True:
        command, arg = recv_uci(p)

        if command == "uciok":
            return engine_info, variants
        elif command == "id":
            name_and_value = arg.split(None, 1)
            if len(name_and_value) == 2:
                engine_info[name_and_value[0]] = name_and_value[1]
        elif command == "option":
            if arg.startswith("name UCI_Variant type combo default chess"):
                for variant in arg.split(" ")[6:]:
                    if variant != "var":
                        variants.add(variant)
        elif command == "Stockfish" and " by " in arg:
            # Ignore identification line
            pass
        else:
            logging.warning("Unexpected engine response to uci: %s %s", command, arg)


def isready(p):
    send(p, "isready")
    while True:
        command, arg = recv_uci(p)
        if command == "readyok":
            break
        elif command == "info" and arg.startswith("string "):
            pass
        else:
            logging.warning("Unexpected engine response to isready: %s %s", command, arg)


def setoption(p, name, value):
    if value is True:
        value = "true"
    elif value is False:
        value = "false"
    elif value is None:
        value = "none"

    send(p, "setoption name %s value %s" % (name, value))


def go(p, position, moves, movetime=None, clock=None, depth=None, nodes=None):
    send(p, "position fen %s moves %s" % (position, " ".join(moves)))

    builder = []
    builder.append("go")
    if movetime is not None:
        builder.append("movetime")
        builder.append(str(movetime))
    if depth is not None:
        builder.append("depth")
        builder.append(str(depth))
    if nodes is not None:
        builder.append("nodes")
        builder.append(str(nodes))
    if clock is not None:
        builder.append("wtime")
        builder.append(str(clock["wtime"] * 10))
        builder.append("btime")
        builder.append(str(clock["btime"] * 10))
        builder.append("winc")
        builder.append(str(clock["inc"] * 1000))
        builder.append("binc")
        builder.append(str(clock["inc"] * 1000))

    send(p, " ".join(builder))

    info = {}
    info["bestmove"] = None

    while True:
        command, arg = recv_uci(p)

        if command == "bestmove":
            bestmove = arg.split()[0]
            if bestmove and bestmove != "(none)":
                info["bestmove"] = bestmove
            return info
        elif command == "info":
            arg = arg or ""

            # Parse all other parameters
            score_kind, score_value, lowerbound, upperbound = None, None, False, False
            current_parameter = None
            for token in arg.split(" "):
                if current_parameter == "string":
                    # Everything until the end of line is a string
                    if "string" in info:
                        info["string"] += " " + token
                    else:
                        info["string"] = token
                elif token == "score":
                    current_parameter = "score"
                elif token == "pv":
                    current_parameter = "pv"
                    if info.get("multipv", 1) == 1:
                        info.pop("pv", None)
                elif token in ["depth", "seldepth", "time", "nodes", "multipv",
                               "currmove", "currmovenumber",
                               "hashfull", "nps", "tbhits", "cpuload",
                               "refutation", "currline", "string"]:
                    current_parameter = token
                    info.pop(current_parameter, None)
                elif current_parameter in ["depth", "seldepth", "time",
                                           "nodes", "currmovenumber",
                                           "hashfull", "nps", "tbhits",
                                           "cpuload", "multipv"]:
                    # Integer parameters
                    info[current_parameter] = int(token)
                elif current_parameter == "score":
                    # Score
                    if token in ["cp", "mate"]:
                        score_kind = token
                        score_value = None
                    elif token == "lowerbound":
                        lowerbound = True
                    elif token == "upperbound":
                        upperbound = True
                    else:
                        score_value = int(token)
                elif current_parameter != "pv" or info.get("multipv", 1) == 1:
                    # Strings
                    if current_parameter in info:
                        info[current_parameter] += " " + token
                    else:
                        info[current_parameter] = token

            # Set score. Prefer scores that are not just a bound
            if score_kind and score_value is not None and (not (lowerbound or upperbound) or "score" not in info or info["score"].get("lowerbound") or info["score"].get("upperbound")):
                info["score"] = {score_kind: score_value}
                if lowerbound:
                    info["score"]["lowerbound"] = lowerbound
                if upperbound:
                    info["score"]["upperbound"] = upperbound
        else:
            logging.warning("Unexpected engine response to go: %s %s", command, arg)


def set_variant_options(p, variant):
    variant = variant.lower()

    setoption(p, "UCI_Chess960", variant in ["fromposition", "chess960"])

    if variant in ["standard", "fromposition", "chess960"]:
        setoption(p, "UCI_Variant", "chess")
    elif variant == "antichess":
        setoption(p, "UCI_Variant", "giveaway")
    elif variant == "threecheck":
        setoption(p, "UCI_Variant", "3check")
    else:
        setoption(p, "UCI_Variant", variant)


class ProgressReporter(threading.Thread):
    def __init__(self, queue_size, conf):
        super(ProgressReporter, self).__init__()
        self.http = requests.Session()
        self.conf = conf

        self.queue = queue.Queue(maxsize=queue_size)
        self._poison_pill = object()

    def send(self, job, result):
        path = "analysis/%s" % job["work"]["id"]
        data = json.dumps(result).encode("utf-8")
        try:
            self.queue.put_nowait((path, data))
        except queue.Full:
            logging.debug("Could not keep up with progress reports. Progress reports are expendable. Dropping one.")

    def stop(self):
        while not self.queue.empty():
            self.queue.get_nowait()
        self.queue.put(self._poison_pill)

    def run(self):
        while True:
            item = self.queue.get()
            if item == self._poison_pill:
                return

            path, data = item

            try:
                response = self.http.post(get_endpoint(self.conf, path),
                                          data=data,
                                          timeout=HTTP_TIMEOUT)
                if response.status_code == 429:
                    logging.error("Too many requests. Suspending progress reports for 60s ...")
                    time.sleep(60.0)
                elif response.status_code != 204:
                    logging.error("Expected status 204 for progress report, got %d. Progress reports are expandable.", response.status_code)
            except requests.RequestException as err:
                logging.warning("Could not send progress report (%s). Progress reports are expendable. Continuing.", err)


class Worker(threading.Thread):
    def __init__(self, conf, threads, memory, user_backlog, system_backlog, progress_reporter):
        super(Worker, self).__init__()
        self.conf = conf
        self.threads = threads
        self.memory = memory
        self.user_backlog = user_backlog
        self.system_backlog = system_backlog
        self.slow = MAX_BACKOFF

        self.progress_reporter = progress_reporter

        self.alive = True
        self.fatal_error = None
        self.finished = threading.Event()
        self.sleep = threading.Event()
        self.status_lock = threading.RLock()

        self.nodes = 0
        self.positions = 0

        self.stockfish_lock = threading.RLock()
        self.stockfish = None
        self.stockfish_info = None

        self.job = None
        self.backoff = start_backoff(self.conf)

        self.http = requests.Session()
        self.http.mount("http://", requests.adapters.HTTPAdapter(max_retries=1))
        self.http.mount("https://", requests.adapters.HTTPAdapter(max_retries=1))

    def set_name(self, name):
        self.name = name
        self.progress_reporter.name = "%s (P)" % (name, )

    def stop(self):
        with self.status_lock:
            self.alive = False
            self.kill_stockfish()
            self.sleep.set()

    def stop_soon(self):
        with self.status_lock:
            self.alive = False
            self.sleep.set()

    def is_alive(self):
        with self.status_lock:
            return self.alive

    def run(self):
        try:
            while self.is_alive():
                self.run_inner()
        except UpdateRequired as error:
            self.fatal_error = error
        except Exception as error:
            self.fatal_error = error
            logging.exception("Fatal error in worker")
        finally:
            self.finished.set()

    def run_inner(self):
        try:
            # Check if the engine is still alive and start, if necessary
            self.start_stockfish()

            # Do the next work unit
            path, request = self.work()
        except DEAD_ENGINE_ERRORS:
            alive = self.is_alive()
            if alive:
                t = next(self.backoff)
                logging.exception("Engine process has died. Backing off %0.1fs", t)

            # Abort current job
            self.abort_job()

            if alive:
                self.sleep.wait(t)
                self.kill_stockfish()

            return

        try:
            # Determine extra wait time based on queue status
            backlog_wait, slow = self.backlog_wait_time()

            params = {}
            if not self.is_alive() or backlog_wait > 0:
                params["stop"] = "true"
            if slow:
                params["slow"] = "true"

            # Report result and fetch next job
            if path != "acquire" or "stop" not in params:
                response = self.http.post(get_endpoint(self.conf, path),
                                          params=params,
                                          json=request,
                                          timeout=HTTP_TIMEOUT)
            else:
                response = None
        except requests.RequestException as err:
            self.job = None
            t = next(self.backoff)
            logging.error("Backing off %0.1fs after failed request (%s)", t, err)
            self.sleep.wait(t)
        else:
            # Handle response.
            if response is None or response.status_code == 204:
                self.job = None
                t = next(self.backoff)
                logging.debug("No job received. Backing off %0.1fs", t)
                self.sleep.wait(t)
            elif response.status_code == 202:
                logging.debug("Got job: %s", response.text)
                self.job = response.json()
                self.backoff = start_backoff(self.conf)
            elif 500 <= response.status_code <= 599:
                self.job = None
                t = next(self.backoff)
                logging.error("Server error: HTTP %d %s. Backing off %0.1fs", response.status_code, response.reason, t)
                self.sleep.wait(t)
            elif 400 <= response.status_code <= 499:
                self.job = None
                t = next(self.backoff) + (60 if response.status_code == 429 else 0)
                try:
                    logging.debug("Client error: HTTP %d %s: %s", response.status_code, response.reason, response.text)
                    error = response.json()["error"]
                    logging.error(error)

                    if "Please restart fishnet to upgrade." in error:
                        logging.error("Stopping worker for update.")
                        raise UpdateRequired()
                except (KeyError, ValueError):
                    logging.error("Client error: HTTP %d %s. Backing off %0.1fs. Request was: %s",
                                  response.status_code, response.reason, t, json.dumps(request))
                self.sleep.wait(t)
            else:
                self.job = None
                t = next(self.backoff)
                logging.error("Unexpected HTTP status for acquire: %d", response.status_code)
                self.sleep.wait(t)

            # Idle the client for some time.
            if self.is_alive() and self.job is None and backlog_wait > 0:
                if backlog_wait >= 120:
                    logging.info("Going idle for %dm", round(backlog_wait / 60))
                else:
                    logging.log(logging.INFO if backlog_wait >= 10 else logging.DEBUG,
                                "Going idle for %0.1fs", backlog_wait)
                self.sleep.wait(backlog_wait)

    def backlog_wait_time(self):
        if not self.is_alive():
            return 0, False

        user_backlog = self.user_backlog + self.slow
        if user_backlog >= 1 or self.system_backlog >= 1:
            try:
                response = self.http.get(get_endpoint(self.conf, "status"), timeout=HTTP_TIMEOUT)
                if response.status_code == 200:
                    status = response.json()
                    user_oldest = max(0, status["analysis"]["user"]["oldest"])
                    user_wait = max(0, user_backlog - user_oldest)
                    system_oldest = max(0, status["analysis"]["system"]["oldest"])
                    system_wait = max(0, self.system_backlog - system_oldest)
                    logging.debug("User wait: %0.1fs due to %0.1fs for oldest %0.1fs, system wait: %0.1fs due to %0.1fs for oldest %0.1fs",
                                  user_wait, user_backlog, user_oldest,
                                  system_wait, self.system_backlog, system_oldest)
                    slow = user_wait >= system_wait + 1.0
                    return min(user_wait, system_wait), slow
                elif response.status_code == 404:
                    # Status deliberately not implemented (for example lila-fishnet)
                    return 0, False
                elif response.status_code == 429:
                    logging.error("Too many requests while checking queue status. Waiting 60s ...")
                    self.sleep.wait(60)
                else:
                    logging.error("Unexpected HTTP status while checking queue status: %d", response.status_code)
            except requests.RequestException:
                logging.error("Could not get status. Continuing.")
            except KeyError:
                logging.warning("Incompatible status response. Continuing.")
        return 0, self.slow >= 1.0

    def abort_job(self):
        if self.job is None:
            return

        logging.debug("Aborting job %s", self.job["work"]["id"])

        try:
            response = requests.post(get_endpoint(self.conf, "abort/%s" % self.job["work"]["id"]),
                                     data=json.dumps(self.make_request()),
                                     timeout=HTTP_TIMEOUT)
            if response.status_code == 204:
                logging.info("Aborted job %s", self.job["work"]["id"])
            else:
                logging.error("Unexpected HTTP status for abort: %d", response.status_code)
        except requests.RequestException:
            logging.exception("Could not abort job. Continuing.")

        self.job = None

    def kill_stockfish(self):
        with self.stockfish_lock:
            if self.stockfish:
                try:
                    kill_process(self.stockfish)
                except OSError:
                    logging.exception("Failed to kill engine process.")
                self.stockfish = None

    def start_stockfish(self):
        with self.stockfish_lock:
            # Check if already running.
            if self.stockfish and self.stockfish.poll() is None:
                return

            # Start process
            self.stockfish = open_process(get_stockfish_command(self.conf, False),
                                          get_engine_dir(self.conf))

        self.stockfish_info, _ = uci(self.stockfish)
        self.stockfish_info.pop("author", None)
        logging.info("Started %s, threads: %s (%d), pid: %d",
                     self.stockfish_info.get("name", "Stockfish <?>"),
                     "+" * self.threads, self.threads, self.stockfish.pid)

        # Prepare UCI options
        self.stockfish_info["options"] = {}
        self.stockfish_info["options"]["threads"] = str(self.threads)
        self.stockfish_info["options"]["hash"] = str(self.memory)
        self.stockfish_info["options"]["analysis contempt"] = "Off"

        # Custom options
        if self.conf.has_section("Stockfish"):
            for name, value in self.conf.items("Stockfish"):
                self.stockfish_info["options"][name] = value

        # Set UCI options
        for name, value in self.stockfish_info["options"].items():
            setoption(self.stockfish, name, value)

        isready(self.stockfish)

    def make_request(self):
        return {
            "fishnet": {
                "version": __version__,
                "python": platform.python_version(),
                "apikey": get_key(self.conf),
            },
            "stockfish": self.stockfish_info,
        }

    def work(self):
        result = self.make_request()

        if self.job and self.job["work"]["type"] == "analysis":
            result = self.analysis(self.job)
            return "analysis" + "/" + self.job["work"]["id"], result
        elif self.job and self.job["work"]["type"] == "move":
            result = self.bestmove(self.job)
            return "move" + "/" + self.job["work"]["id"], result
        else:
            if self.job:
                logging.error("Invalid job type: %s", self.job["work"]["type"])

            return "acquire", result

    def job_name(self, job, ply=None):
        builder = []
        if job.get("game_id"):
            builder.append(base_url(get_endpoint(self.conf)))
            builder.append(job["game_id"])
        else:
            builder.append(job["work"]["id"])
        if ply is not None:
            builder.append("#")
            builder.append(str(ply))
        return "".join(builder)

    def bestmove(self, job):
        lvl = job["work"]["level"]
        variant = job.get("variant", "standard")
        moves = job["moves"].split(" ")

        logging.debug("Playing %s (%s) with lvl %d",
                      self.job_name(job), variant, lvl)

        set_variant_options(self.stockfish, job.get("variant", "standard"))
        setoption(self.stockfish, "Skill Level", LVL_SKILL[lvl - 1])
        setoption(self.stockfish, "UCI_AnalyseMode", False)
        send(self.stockfish, "ucinewgame")
        isready(self.stockfish)

        movetime = int(round(LVL_MOVETIMES[lvl - 1] / (self.threads * 0.9 ** (self.threads - 1))))

        start = time.time()
        part = go(self.stockfish, job["position"], moves,
                  movetime=movetime, clock=job["work"].get("clock"),
                  depth=LVL_DEPTHS[lvl - 1])
        end = time.time()

        logging.log(PROGRESS, "Played move in %s (%s) with lvl %d: %0.3fs elapsed, depth %d",
                    self.job_name(job), variant,
                    lvl, end - start, part.get("depth", 0))

        self.slow = 0.1  # move clients are trusted to be fast

        self.nodes += part.get("nodes", 0)
        self.positions += 1

        result = self.make_request()
        result["move"] = {
            "bestmove": part["bestmove"],
        }
        return result

    def analysis(self, job):
        variant = job.get("variant", "standard")
        moves = job["moves"].split(" ")

        result = self.make_request()
        result["analysis"] = [None for _ in range(len(moves) + 1)]
        start = last_progress_report = time.time()

        set_variant_options(self.stockfish, variant)
        setoption(self.stockfish, "Skill Level", 20)
        setoption(self.stockfish, "UCI_AnalyseMode", True)
        send(self.stockfish, "ucinewgame")
        isready(self.stockfish)

        nodes = job.get("nodes") or 3500000
        skip = job.get("skipPositions", [])

        num_positions = 0

        for ply in range(len(moves), -1, -1):
            if ply in skip:
                result["analysis"][ply] = {"skipped": True}
                continue

            if last_progress_report + PROGRESS_REPORT_INTERVAL < time.time():
                if self.progress_reporter:
                    self.progress_reporter.send(job, result)
                last_progress_report = time.time()

            logging.log(PROGRESS, "Analysing %s: %s",
                        variant, self.job_name(job, ply))

            part = go(self.stockfish, job["position"], moves[0:ply],
                      nodes=nodes, movetime=int(MAX_MOVE_TIME * 1000))

            if part["score"].get("cp") and "time" in part and part["time"] < 100:
                logging.warning("Very low time reported: %d ms.", part["time"])

            if "nps" in part and part["nps"] >= 100000000:
                logging.warning("Dropping exorbitant nps: %d", part["nps"])
                del part["nps"]

            self.nodes += part.get("nodes", 0)
            self.positions += 1
            num_positions += 1

            result["analysis"][ply] = part

        end = time.time()

        if num_positions:
            t = (end - start) / num_positions
            logging.info("%s took %0.1fs (%0.1fs per position)",
                         self.job_name(job),
                         end - start, t)
            if t > 0.95 * MAX_MOVE_TIME:
                logging.warning("Extremely slow (%0.1fs per position). If this happens frequently, it is better to stop and defer to clients with better hardware.", t)
            elif t > TARGET_MOVE_TIME + 0.1 and self.slow < MAX_SLOW_BACKOFF:
                self.slow = min(self.slow * 2, MAX_SLOW_BACKOFF)
                logging.info("Slower than %0.1fs per position (%0.1fs). Will accept only older user requests (backlog >= %0.1fs).", TARGET_MOVE_TIME, t, self.slow)
            elif t < TARGET_MOVE_TIME - 0.1 and self.slow > 0.1:
                self.slow = max(self.slow / 2, 0.1)
                if self.is_alive() and self.slow > 0.5:
                    logging.info("Nice, faster than %0.1fs per position (%0.1fs)! Will accept younger user requests (backlog >= %0.1fs).", TARGET_MOVE_TIME, t, self.slow)
                else:
                    logging.debug("Nice, faster than %0.1fs per position (%0.1fs)! More confident in performance (backlog >= %0.1fs).", TARGET_MOVE_TIME, t, self.slow)
        else:
            logging.info("%s done (nothing to do)", self.job_name(job))

        return result


def detect_cpu_capabilities():
    # Detects support for popcnt and pext instructions
    vendor, modern, bmi2 = "", False, False

    # Run cpuid in subprocess for robustness in case of segfaults
    cmd = []
    cmd.append(sys.executable)
    if __package__ is not None:
        cmd.append("-m")
        cmd.append(os.path.splitext(os.path.basename(__file__))[0])
    else:
        cmd.append(__file__)
    cmd.append("cpuid")

    process = open_process(cmd, shell=False)

    # Parse output
    while True:
        line = process.stdout.readline()
        if not line:
            break

        line = line.rstrip()
        logging.debug("cpuid >> %s", line)
        if not line:
            continue

        columns = line.split()
        if columns[0] == "CPUID":
            pass
        elif len(columns) == 5 and all(all(c in string.hexdigits for c in col) for col in columns):
            eax, a, b, c, d = [int(col, 16) for col in columns]

            # vendor
            if eax == 0:
                vendor = struct.pack("III", b, d, c).decode("utf-8")

            # popcnt
            if eax == 1 and c & (1 << 23):
                modern = True

            # pext
            if eax == 7 and b & (1 << 8):
                bmi2 = True
        else:
            logging.warning("Unexpected cpuid output: %s", line)

    # Done
    process.communicate()
    if process.returncode != 0:
        logging.error("cpuid exited with status code %d", process.returncode)

    return vendor, modern, bmi2


def stockfish_filename():
    machine = platform.machine().lower()

    vendor, modern, bmi2 = detect_cpu_capabilities()
    if modern and "Intel" in vendor and bmi2:
        suffix = "-bmi2"
    elif modern:
        suffix = "-modern"
    else:
        suffix = ""

    if os.name == "nt":
        return "stockfish-windows-%s%s.exe" % (machine, suffix)
    elif os.name == "os2" or sys.platform == "darwin":
        return "stockfish-osx-%s" % machine
    elif os.name == "posix":
        return "stockfish-%s%s" % (machine, suffix)


def download_github_release(conf, release_page, filename):
    path = os.path.join(get_engine_dir(conf), filename)
    logging.info("Engine target path: %s", path)

    headers = {}

    # Only update to newer versions
    try:
        headers["If-Modified-Since"] = time.strftime('%a, %d %b %Y %H:%M:%S GMT', time.gmtime(os.path.getmtime(path)))
    except OSError:
        pass

    # Escape GitHub API rate limiting
    if "GITHUB_API_TOKEN" in os.environ:
        headers["Authorization"] = "token %s" % os.environ["GITHUB_API_TOKEN"]

    # Find latest release
    logging.info("Looking up %s ...", filename)

    response = requests.get(release_page, headers=headers, timeout=HTTP_TIMEOUT)
    if response.status_code == 304:
        logging.info("Local %s is newer than release", filename)
        return filename
    elif response.status_code != 200:
        raise ConfigError("Failed to look up latest Stockfish release (status %d)" % (response.status_code, ))

    release = response.json()

    logging.info("Latest release is tagged %s", release["tag_name"])

    for asset in release["assets"]:
        if asset["name"] == filename:
            logging.info("Found %s" % asset["browser_download_url"])
            break
    else:
        raise ConfigError("No precompiled %s for your platform" % filename)

    # Download
    logging.info("Downloading %s ...", filename)

    download = requests.get(asset["browser_download_url"], stream=True, timeout=HTTP_TIMEOUT)
    progress = 0
    size = int(download.headers["content-length"])
    with open(path, "wb") as target:
        for chunk in download.iter_content(chunk_size=1024):
            target.write(chunk)
            progress += len(chunk)

            if sys.stderr.isatty():
                sys.stderr.write("\rDownloading %s: %d/%d (%d%%)" % (
                                    filename, progress, size,
                                    progress * 100 / size))
                sys.stderr.flush()
    if sys.stderr.isatty():
        sys.stderr.write("\n")
        sys.stderr.flush()

    # Make executable
    logging.info("chmod +x %s", filename)
    st = os.stat(path)
    os.chmod(path, st.st_mode | stat.S_IEXEC)
    return filename


def update_stockfish(conf, filename):
    return download_github_release(conf, STOCKFISH_RELEASES, filename)


def is_user_site_package():
    try:
        user_site = site.getusersitepackages()
    except AttributeError:
        return False

    return os.path.abspath(__file__).startswith(os.path.join(user_site, ""))


def update_self():
    # Ensure current instance is installed as a package
    if __package__ is None:
        raise ConfigError("Not started as a package (python -m). Cannot update using pip")

    if all(dirname not in ["site-packages", "dist-packages"] for dirname in __file__.split(os.sep)):
        raise ConfigError("Not installed as package (%s). Cannot update using pip" % __file__)

    logging.debug("Package: \"%s\", name: %s, loader: %s",
                  __package__, __name__, __loader__)

    # Ensure pip is available
    try:
        pip_info = subprocess.check_output([sys.executable, "-m", "pip", "--version"],
                                           universal_newlines=True)
    except OSError:
        raise ConfigError("Auto update enabled, but cannot run pip")
    else:
        logging.debug("Pip: %s", pip_info.rstrip())

    # Ensure module file is going to be writable
    try:
        with open(__file__, "r+"):
            pass
    except IOError:
        raise ConfigError("Auto update enabled, but no write permissions "
                          "to module file. Use virtualenv or "
                          "pip install --user")

    # Look up the latest version
    result = requests.get("https://pypi.org/pypi/fishnet/json", timeout=HTTP_TIMEOUT).json()
    latest_version = result["info"]["version"]
    url = result["releases"][latest_version][0]["url"]
    if latest_version == __version__:
        logging.info("Already up to date.")
        return 0

    # Wait
    t = random.random() * 15.0
    logging.info("Waiting %0.1fs before update ...", t)
    time.sleep(t)

    print()

    # Update
    if is_user_site_package():
        logging.info("$ pip install --user --upgrade %s", url)
        ret = subprocess.call([sys.executable, "-m", "pip", "install", "--user", "--upgrade", url],
                              stdout=sys.stdout, stderr=sys.stderr)
    else:
        logging.info("$ pip install --upgrade %s", url)
        ret = subprocess.call([sys.executable, "-m", "pip", "install", "--upgrade", url],
                              stdout=sys.stdout, stderr=sys.stderr)
    if ret != 0:
        logging.warning("Unexpected exit code for pip install: %d", ret)
        return ret

    print()

    # Wait
    t = random.random() * 15.0
    logging.info("Waiting %0.1fs before respawn ...", t)
    time.sleep(t)

    # Respawn
    argv = []
    argv.append(sys.executable)
    argv.append("-m")
    argv.append(os.path.splitext(os.path.basename(__file__))[0])
    argv += sys.argv[1:]

    logging.debug("Restarting with execv: %s, argv: %s",
                  sys.executable, " ".join(argv))

    os.execv(sys.executable, argv)


def load_conf(args):
    conf = configparser.ConfigParser()
    conf.add_section("Fishnet")
    conf.add_section("Stockfish")

    if not args.no_conf:
        if not args.conf and not os.path.isfile(DEFAULT_CONFIG):
            conf = configure(args)
        else:
            config_file = args.conf or DEFAULT_CONFIG
            logging.debug("Using config file: %s", config_file)

            if not conf.read(config_file):
                raise ConfigError("Could not read config file: %s" % config_file)

    if hasattr(args, "engine_dir") and args.engine_dir is not None:
        conf.set("Fishnet", "EngineDir", args.engine_dir)
    if hasattr(args, "stockfish_command") and args.stockfish_command is not None:
        conf.set("Fishnet", "StockfishCommand", args.stockfish_command)
    if hasattr(args, "key") and args.key is not None:
        conf.set("Fishnet", "Key", args.key)
    if hasattr(args, "cores") and args.cores is not None:
        conf.set("Fishnet", "Cores", args.cores)
    if hasattr(args, "memory") and args.memory is not None:
        conf.set("Fishnet", "Memory", args.memory)
    if hasattr(args, "threads_per_process") and args.threads_per_process is not None:
        conf.set("Fishnet", "ThreadsPerProcess", str(args.threads_per_process))
    if hasattr(args, "endpoint") and args.endpoint is not None:
        conf.set("Fishnet", "Endpoint", args.endpoint)
    if hasattr(args, "fixed_backoff") and args.fixed_backoff is not None:
        conf.set("Fishnet", "FixedBackoff", str(args.fixed_backoff))
    if hasattr(args, "user_backlog") and args.user_backlog is not None:
        conf.set("Fishnet", "UserBacklog", args.user_backlog)
    if hasattr(args, "system_backlog") and args.system_backlog is not None:
        conf.set("Fishnet", "SystemBacklog", args.system_backlog)
    for option_name, option_value in args.setoption:
        conf.set("Stockfish", option_name.lower(), option_value)

    logging.getLogger().addFilter(CensorLogFilter(conf_get(conf, "Key")))

    return conf


def config_input(prompt, validator, out):
    while True:
        if out == sys.stdout:
            inp = input(prompt)
        else:
            if prompt:
                out.write(prompt)
                out.flush()

            inp = input()

        try:
            return validator(inp)
        except ConfigError as error:
            print(error, file=out)


def configure(args):
    if sys.stdout.isatty():
        out = sys.stdout
        try:
            # Unix: Importing for its side effect
            import readline  # noqa: F401
        except ImportError:
            # Windows
            pass
    else:
        out = sys.stderr

    print(file=out)
    print("### Configuration", file=out)
    print(file=out)

    conf = configparser.ConfigParser()
    conf.add_section("Fishnet")
    conf.add_section("Stockfish")

    # Ensure the config file is going to be writable
    config_file = os.path.abspath(args.conf or DEFAULT_CONFIG)
    if os.path.isfile(config_file):
        conf.read(config_file)
        with open(config_file, "r+"):
            pass
    else:
        with open(config_file, "w"):
            pass
        os.remove(config_file)

    # Stockfish working directory
    if args.engine_dir is None:
        engine_dir = config_input("Engine working directory (default: %s): " % os.path.abspath("."),
                                  validate_engine_dir, out)
    else:
        engine_dir = validate_engine_dir(args.engine_dir)
        print("Engine working directory: %s" % engine_dir, file=out)
    conf.set("Fishnet", "EngineDir", engine_dir)

    # Stockfish command
    print(file=out)
    print("Fishnet uses a custom Stockfish build with variant support.", file=out)
    print("Stockfish is licensed under the GNU General Public License v3.", file=out)
    print("You can find the source at: https://github.com/ddugovic/Stockfish", file=out)
    print(file=out)
    print("You can build lichess.org custom Stockfish yourself and provide", file=out)
    print("the path or automatically download a precompiled binary.", file=out)
    print(file=out)
    if args.stockfish_command is None:
        stockfish_command = config_input("Path or command (will download by default): ",
                                         lambda v: validate_stockfish_command(v, conf),
                                         out)
    else:
        stockfish_command = validate_stockfish_command(args.stockfish_command, conf)
        print("Path or command: %s" % stockfish_command, file=out)
    if not stockfish_command:
        conf.remove_option("Fishnet", "StockfishCommand")
    else:
        conf.set("Fishnet", "StockfishCommand", stockfish_command)
    print(file=out)

    # Cores
    if args.cores is None:
        max_cores = multiprocessing.cpu_count()
        default_cores = max(1, max_cores - 1)
        cores = config_input("Number of cores to use for engine threads (default %d, max %d): " % (default_cores, max_cores),
                             validate_cores, out)
    else:
        cores = validate_cores(args.cores)
        print("Number of cores to use for engine threads: %d" % cores, file=out)
    conf.set("Fishnet", "Cores", str(cores))

    # Backlog
    if args.user_backlog is None and args.system_backlog is None:
        print(file=out)
        print("You can choose to join only if a backlog is building up. Examples:", file=out)
        print("* Rented server exlusively for fishnet: choose no", file=out)
        print("* Running on laptop: choose yes", file=out)
        if config_input("Would you prefer to keep your client idle? (default: no) ", parse_bool, out):
            conf.set("Fishnet", "UserBacklog", "short")
            conf.set("Fishnet", "SystemBacklog", "long")
        else:
            conf.set("Fishnet", "UserBacklog", "0s")
            conf.set("Fishnet", "SystemBacklog", "0s")
        print(file=out)
    else:
        print("Using custom backlog parameters.", file=out)
        if args.user_backlog is not None:
            parse_duration(args.user_backlog)
            conf.set("Fishnet", "UserBacklog", args.user_backlog)
        if args.system_backlog is not None:
            parse_duration(args.system_backlog)
            conf.set("Fishnet", "SystemBacklog", args.system_backlog)

    # Advanced options
    if args.endpoint is None:
        if config_input("Configure advanced options? (default: no) ", parse_bool, out):
            endpoint = config_input("Fishnet API endpoint (default: %s): " % DEFAULT_ENDPOINT, validate_endpoint, out)
        else:
            endpoint = DEFAULT_ENDPOINT
    else:
        endpoint = validate_endpoint(args.endpoint)
        print("Fishnet API endpoint: %s" % endpoint, file=out)
    conf.set("Fishnet", "Endpoint", endpoint)

    # Change key?
    if args.key is None:
        key = None
        if conf.has_option("Fishnet", "Key"):
            if not config_input("Change fishnet key? (default: no) ", parse_bool, out):
                key = conf.get("Fishnet", "Key")

        # Key
        if key is None:
            status = "https://lichess.org/get-fishnet" if is_production_endpoint(conf) else "probably not required"
            key = config_input("Personal fishnet key (append ! to force, %s): " % status,
                               lambda v: validate_key(v, conf, network=True), out)
    else:
        key = validate_key(args.key, conf, network=True)
        print("Personal fishnet key: %s" % (("*" * len(key) or "(none)", )), file=out)
    conf.set("Fishnet", "Key", key)
    logging.getLogger().addFilter(CensorLogFilter(key))

    # Confirm
    print(file=out)
    while not config_input("Done. Write configuration to %s now? (default: yes) " % (config_file, ),
                           lambda v: parse_bool(v, True), out):
        pass

    # Write configuration
    with open(config_file, "w") as f:
        conf.write(f)

    print("Configuration saved.", file=out)
    return conf


def validate_engine_dir(engine_dir):
    if not engine_dir or not engine_dir.strip():
        return os.path.abspath(".")

    engine_dir = os.path.abspath(os.path.expanduser(engine_dir.strip()))

    if not os.path.isdir(engine_dir):
        raise ConfigError("EngineDir not found: %s" % engine_dir)

    return engine_dir


def validate_stockfish_command(stockfish_command, conf):
    if not stockfish_command or not stockfish_command.strip() or stockfish_command.strip().lower() == "download":
        return None

    stockfish_command = stockfish_command.strip()
    engine_dir = get_engine_dir(conf)

    # Ensure the required options are supported
    process = open_process(stockfish_command, engine_dir)
    _, variants = uci(process)
    kill_process(process)

    logging.debug("Supported variants: %s", ", ".join(variants))

    required_variants = set(["chess", "giveaway", "atomic", "crazyhouse", "horde",  "kingofthehill", "racingkings", "3check"])
    missing_variants = required_variants.difference(variants)
    if missing_variants:
        raise ConfigError("Ensure you are using lichess custom Stockfish. "
                          "Unsupported variants: %s" % ", ".join(missing_variants))

    return stockfish_command


def parse_bool(inp, default=False):
    if not inp:
        return default

    inp = inp.strip().lower()
    if not inp:
        return default

    if inp in ["y", "j", "yes", "yep", "true", "t", "1", "ok"]:
        return True
    elif inp in ["n", "no", "nop", "nope", "f", "false", "0"]:
        return False
    else:
        raise ConfigError("Not a boolean value: %s" % (inp, ))


def parse_duration(inp, default=0):
    if not inp:
        return default

    stripped = inp.strip().lower()
    if not stripped:
        return default
    elif stripped == "short":
        return 60
    elif stripped == "long":
        return 60 * 60

    factor = 1
    if stripped.endswith("s"):
        stripped = stripped[:-1].rstrip()
    elif inp.endswith("m"):
        factor = 60
        stripped = stripped[:-1].rstrip()
    elif inp.endswith("h"):
        factor = 60 * 60
        stripped = stripped[:-1].rstrip()
    elif inp.endswith("d"):
        factor = 60 * 60 * 24
        stripped = stripped[:-1].rstrip()

    try:
        return int(stripped) * factor
    except ValueError:
        raise ConfigError("Not a valid duration: %s" % (inp, ))


def validate_backlog(conf):
    return parse_duration(conf_get(conf, "UserBacklog")), parse_duration(conf_get(conf, "SystemBacklog"))


def validate_cores(cores):
    if not cores or cores.strip().lower() == "auto":
        return max(1, multiprocessing.cpu_count() - 1)

    if cores.strip().lower() == "all":
        return multiprocessing.cpu_count()

    try:
        cores = int(cores.strip())
    except ValueError:
        raise ConfigError("Number of cores must be an integer")

    if cores < 1:
        raise ConfigError("Need at least one core")

    if cores > multiprocessing.cpu_count():
        raise ConfigError("At most %d cores available on your machine " % multiprocessing.cpu_count())

    return cores


def validate_threads_per_process(threads, conf):
    cores = validate_cores(conf_get(conf, "Cores"))

    if not threads or str(threads).strip().lower() == "auto":
        return min(DEFAULT_THREADS, cores)

    try:
        threads = int(str(threads).strip())
    except ValueError:
        raise ConfigError("Number of threads must be an integer")

    if threads < 1:
        raise ConfigError("Need at least one thread per engine process")

    if threads > cores:
        raise ConfigError("%d cores is not enough to run %d threads" % (cores, threads))

    return threads


def validate_memory(memory, conf):
    cores = validate_cores(conf_get(conf, "Cores"))
    threads = validate_threads_per_process(conf_get(conf, "ThreadsPerProcess"), conf)
    processes = (cores-1) // threads + 1

    if not memory or not memory.strip() or memory.strip().lower() == "auto":
        return processes * HASH_DEFAULT

    try:
        memory = int(memory.strip())
    except ValueError:
        raise ConfigError("Memory must be an integer")

    if memory < processes * HASH_MIN:
        raise ConfigError("Not enough memory for a minimum of %d x %d MB in hash tables" % (processes, HASH_MIN))

    if memory > processes * HASH_MAX:
        raise ConfigError("Cannot reasonably use more than %d x %d MB = %d MB for hash tables" % (processes, HASH_MAX, processes * HASH_MAX))

    return memory


def validate_endpoint(endpoint, default=DEFAULT_ENDPOINT):
    if not endpoint or not endpoint.strip():
        return default

    if not endpoint.endswith("/"):
        endpoint += "/"

    url_info = urlparse.urlparse(endpoint)
    if url_info.scheme not in ["http", "https"]:
        raise ConfigError("Endpoint does not have http:// or https:// URL scheme")

    return endpoint


def validate_key(key, conf, network=False):
    if not key or not key.strip():
        if is_production_endpoint(conf):
            raise ConfigError("Fishnet key required")
        else:
            return ""

    key = key.strip()

    network = network and not key.endswith("!")
    key = key.rstrip("!").strip()

    if not re.match(r"^[a-zA-Z0-9]+$", key):
        raise ConfigError("Fishnet key is expected to be alphanumeric")

    if network:
        response = requests.get(get_endpoint(conf, "key/%s" % key), timeout=HTTP_TIMEOUT)
        if response.status_code == 404:
            raise ConfigError("Invalid or inactive fishnet key")
        else:
            response.raise_for_status()

    return key


def conf_get(conf, key, default=None, section="Fishnet"):
    if not conf.has_section(section):
        return default
    elif not conf.has_option(section, key):
        return default
    else:
        return conf.get(section, key)


def get_engine_dir(conf):
    return validate_engine_dir(conf_get(conf, "EngineDir"))


def get_stockfish_command(conf, update=True):
    stockfish_command = validate_stockfish_command(conf_get(conf, "StockfishCommand"), conf)
    if not stockfish_command:
        filename = stockfish_filename()
        if update:
            filename = update_stockfish(conf, filename)
        return validate_stockfish_command(os.path.join(".", filename), conf)
    else:
        return stockfish_command


def get_endpoint(conf, sub=""):
    return urlparse.urljoin(validate_endpoint(conf_get(conf, "Endpoint")), sub)


def is_production_endpoint(conf):
    endpoint = validate_endpoint(conf_get(conf, "Endpoint"))
    hostname = urlparse.urlparse(endpoint).hostname
    return hostname == "lichess.org" or hostname.endswith(".lichess.org")


def get_key(conf):
    return validate_key(conf_get(conf, "Key"), conf, network=False)


def start_backoff(conf):
    if parse_bool(conf_get(conf, "FixedBackoff")):
        while True:
            yield random.random() * MAX_FIXED_BACKOFF
    else:
        backoff = 1
        while True:
            yield 0.5 * backoff + 0.5 * backoff * random.random()
            backoff = min(backoff + 1, MAX_BACKOFF)


def update_available():
    try:
        result = requests.get("https://pypi.org/pypi/fishnet/json", timeout=HTTP_TIMEOUT).json()
        latest_version = result["info"]["version"]
    except Exception:
        logging.exception("Failed to check for update on PyPI")
        return False

    if latest_version == __version__:
        logging.info("[fishnet v%s] Client is up to date", __version__)
        return False
    else:
        logging.info("[fishnet v%s] Update available on PyPI: %s",
                     __version__, latest_version)
        return True


def cmd_run(args):
    conf = load_conf(args)

    if args.auto_update:
        print()
        print("### Updating ...")
        print()
        update_self()

    stockfish_command = validate_stockfish_command(conf_get(conf, "StockfishCommand"), conf)
    if not stockfish_command:
        print()
        print("### Updating Stockfish ...")
        print()
        stockfish_command = get_stockfish_command(conf)

    print()
    print("### Checking configuration ...")
    print()
    print("Python:           %s (with requests %s)" % (platform.python_version(), requests.__version__))
    print("EngineDir:        %s" % get_engine_dir(conf))
    print("StockfishCommand: %s" % stockfish_command)
    print("Key:              %s" % (("*" * len(get_key(conf))) or "(none)"))

    cores = validate_cores(conf_get(conf, "Cores"))
    print("Cores:            %d" % cores)

    threads = validate_threads_per_process(conf_get(conf, "ThreadsPerProcess"), conf)
    instances = max(1, (cores-1) // threads + 1)
    print("Engine processes: %d (each ~%d threads)" % (instances, threads))
    memory = validate_memory(conf_get(conf, "Memory"), conf)
    print("Memory:           %d MB" % memory)
    endpoint = get_endpoint(conf)
    warning = "" if endpoint.startswith("https://") else " (WARNING: not using https)"
    print("Endpoint:         %s%s" % (endpoint, warning))
    user_backlog, system_backlog = validate_backlog(conf)
    print("UserBacklog:      %ds" % user_backlog)
    print("SystemBacklog:    %ds" % system_backlog)
    print("FixedBackoff:     %s" % parse_bool(conf_get(conf, "FixedBackoff")))
    print()

    if conf.has_section("Stockfish") and conf.items("Stockfish"):
        print("Using custom UCI options is discouraged:")
        for name, value in conf.items("Stockfish"):
            if name.lower() == "hash":
                hint = " (use --memory instead)"
            elif name.lower() == "threads":
                hint = " (use --threads-per-process instead)"
            else:
                hint = ""
            print(" * %s = %s%s" % (name, value, hint))
        print()

    if args.ignored_threads:
        print("Ignored deprecated option --threads. Did you mean --cores?")
        print()

    print("### Starting workers (press Ctrl + C to stop) ...")
    print()

    buckets = [0] * instances
    for i in range(0, cores):
        buckets[i % instances] += 1

    progress_reporter = ProgressReporter(len(buckets) + 4, conf)
    progress_reporter.setDaemon(True)
    progress_reporter.start()

    workers = [Worker(conf, bucket, memory // instances, user_backlog, system_backlog, progress_reporter) for bucket in buckets]

    # Start all threads
    for i, worker in enumerate(workers):
        worker.set_name("><> %d" % (i + 1))
        worker.setDaemon(True)
        worker.start()

    # Wait while the workers are running
    try:
        # Let SIGTERM and SIGINT gracefully terminate the program
        handler = SignalHandler()

        try:
            while True:
                # Check worker status
                for _ in range(int(max(1, STAT_INTERVAL / len(workers)))):
                    for worker in workers:
                        worker.finished.wait(1.0)
                        if worker.fatal_error:
                            raise worker.fatal_error

                # Log stats
                logging.info("[fishnet v%s] Analyzed %d positions, crunched %d million nodes",
                             __version__,
                             sum(worker.positions for worker in workers),
                             int(sum(worker.nodes for worker in workers) / 1000 / 1000))

                # Check for update
                if random.random() <= CHECK_PYPI_CHANCE and update_available() and args.auto_update:
                    raise UpdateRequired()
        except ShutdownSoon:
            handler = SignalHandler()

            if any(worker.job for worker in workers):
                logging.info("\n\n### Stopping soon. Press ^C again to abort pending jobs ...\n")

            for worker in workers:
                worker.stop_soon()

            for worker in workers:
                while not worker.finished.wait(0.5):
                    pass
    except (Shutdown, ShutdownSoon):
        if any(worker.job for worker in workers):
            logging.info("\n\n### Good bye! Aborting pending jobs ...\n")
        else:
            logging.info("\n\n### Good bye!")
    except UpdateRequired:
        if any(worker.job for worker in workers):
            logging.info("\n\n### Update required! Aborting pending jobs ...\n")
        else:
            logging.info("\n\n### Update required!")
        raise
    finally:
        handler.ignore = True

        # Stop workers
        for worker in workers:
            worker.stop()

        progress_reporter.stop()

        # Wait
        for worker in workers:
            worker.finished.wait()

    return 0


def cmd_configure(args):
    configure(args)
    return 0


def cmd_systemd(args):
    conf = load_conf(args)

    if args.command == "systemd-user":
        template = textwrap.dedent("""\
            [Unit]
            Description=Fishnet client
            After=network-online.target
            Wants=network-online.target

            [Service]
            ExecStart={start}
            KillMode=mixed
            WorkingDirectory={cwd}
            ReadWriteDirectories={cwd}
            Nice=5
            PrivateTmp=true
            DevicePolicy=closed
            ProtectSystem={protect_system}
            Restart=always

            [Install]
            WantedBy=default.target""")
    else:
        template = textwrap.dedent("""\
            [Unit]
            Description=Fishnet client
            After=network-online.target
            Wants=network-online.target

            [Service]
            ExecStart={start}
            KillMode=mixed
            WorkingDirectory={cwd}
            ReadWriteDirectories={cwd}
            User={user}
            Group={group}
            Nice=5
            CapabilityBoundingSet=
            PrivateTmp=true
            PrivateDevices=true
            DevicePolicy=closed
            ProtectSystem={protect_system}
            NoNewPrivileges=true
            Restart=always

            [Install]
            WantedBy=multi-user.target""")

    # Prepare command line arguments
    builder = [shell_quote(sys.executable)]

    if __package__ is None:
        builder.append(shell_quote(os.path.abspath(sys.argv[0])))
    else:
        builder.append("-m")
        builder.append(shell_quote(os.path.splitext(os.path.basename(__file__))[0]))

    if args.no_conf:
        builder.append("--no-conf")
    else:
        config_file = os.path.abspath(args.conf or DEFAULT_CONFIG)
        builder.append("--conf")
        builder.append(shell_quote(config_file))

    if args.key is not None:
        builder.append("--key")
        builder.append(shell_quote(validate_key(args.key, conf)))
    if args.engine_dir is not None:
        builder.append("--engine-dir")
        builder.append(shell_quote(validate_engine_dir(args.engine_dir)))
    if args.stockfish_command is not None:
        builder.append("--stockfish-command")
        builder.append(shell_quote(validate_stockfish_command(args.stockfish_command, conf)))
    if args.cores is not None:
        builder.append("--cores")
        builder.append(shell_quote(str(validate_cores(args.cores))))
    if args.memory is not None:
        builder.append("--memory")
        builder.append(shell_quote(str(validate_memory(args.memory, conf))))
    if args.threads_per_process is not None:
        builder.append("--threads-per-process")
        builder.append(shell_quote(str(validate_threads_per_process(args.threads_per_process, conf))))
    if args.endpoint is not None:
        builder.append("--endpoint")
        builder.append(shell_quote(validate_endpoint(args.endpoint)))
    if args.fixed_backoff is not None:
        builder.append("--fixed-backoff" if args.fixed_backoff else "--no-fixed-backoff")
    if args.user_backlog is not None:
        builder.append("--user-backlog")
        builder.append("%ds" % parse_duration(args.user_backlog))
    if args.system_backlog is not None:
        builder.append("--system-backlog")
        builder.append("%ds" % parse_duration(args.system_backlog))
    for option_name, option_value in args.setoption:
        builder.append("--setoption")
        builder.append(shell_quote(option_name))
        builder.append(shell_quote(option_value))
    if args.auto_update:
        builder.append("--auto-update")

    builder.append("run")

    start = " ".join(builder)

    protect_system = "full"
    if args.auto_update and os.path.realpath(os.path.abspath(__file__)).startswith("/usr/"):
        protect_system = "false"

    print(template.format(
        user=getpass.getuser(),
        group=getpass.getuser(),
        cwd=os.path.abspath("."),
        start=start,
        protect_system=protect_system
    ))

    try:
        if os.geteuid() == 0:
            print("\n# WARNING: Running as root is not recommended!", file=sys.stderr)
    except AttributeError:
        # No os.getuid() on Windows
        pass

    if sys.stdout.isatty():
        print("\n# Example usage:", file=sys.stderr)
        if args.command == "systemd-user":
            print("# python -m fishnet systemd-user | tee ~/.config/systemd/user/fishnet.service", file=sys.stderr)
            print("# systemctl enable --user fishnet.service", file=sys.stderr)
            print("# systemctl start --user fishnet.service", file=sys.stderr)
            print("#", file=sys.stderr)
            print("# Live view of log: journalctl --follow --user-unit fishnet", file=sys.stderr)
        else:
            print("# python -m fishnet systemd | sudo tee /etc/systemd/system/fishnet.service", file=sys.stderr)
            print("# sudo systemctl enable fishnet.service", file=sys.stderr)
            print("# sudo systemctl start fishnet.service", file=sys.stderr)
            print("#", file=sys.stderr)
            print("# Live view of the log: sudo journalctl --follow -u fishnet", file=sys.stderr)
            print("#", file=sys.stderr)
            print("# Need a user unit? python -m fishnet systemd-user", file=sys.stderr)


@contextlib.contextmanager
def make_cpuid():
    # Loosely based on cpuid.py by Anders Høst, licensed MIT:
    # https://github.com/flababah/cpuid.py

    # Prepare system information
    is_windows = os.name == "nt"
    is_64bit = ctypes.sizeof(ctypes.c_void_p) == 8
    if platform.machine().lower() not in ["amd64", "x86_64", "x86", "i686"]:
        raise OSError("Got no CPUID opcodes for %s" % platform.machine())

    # Struct for return value
    class CPUID_struct(ctypes.Structure):
        _fields_ = [("eax", ctypes.c_uint32),
                    ("ebx", ctypes.c_uint32),
                    ("ecx", ctypes.c_uint32),
                    ("edx", ctypes.c_uint32)]

    # Select kernel32 or libc
    if is_windows:
        libc = ctypes.windll.kernel32
    else:
        libc = ctypes.cdll.LoadLibrary(None)

    # Select opcodes
    if is_64bit:
        if is_windows:
            # Windows x86_64
            # Three first call registers : RCX, RDX, R8
            # Volatile registers         : RAX, RCX, RDX, R8-11
            opc = [
                0x53,                    # push   %rbx
                0x89, 0xd0,              # mov    %edx,%eax
                0x49, 0x89, 0xc9,        # mov    %rcx,%r9
                0x44, 0x89, 0xc1,        # mov    %r8d,%ecx
                0x0f, 0xa2,              # cpuid
                0x41, 0x89, 0x01,        # mov    %eax,(%r9)
                0x41, 0x89, 0x59, 0x04,  # mov    %ebx,0x4(%r9)
                0x41, 0x89, 0x49, 0x08,  # mov    %ecx,0x8(%r9)
                0x41, 0x89, 0x51, 0x0c,  # mov    %edx,0xc(%r9)
                0x5b,                    # pop    %rbx
                0xc3                     # retq
            ]
        else:
            # Posix x86_64
            # Three first call registers : RDI, RSI, RDX
            # Volatile registers         : RAX, RCX, RDX, RSI, RDI, R8-11
            opc = [
                0x53,                    # push   %rbx
                0x89, 0xf0,              # mov    %esi,%eax
                0x89, 0xd1,              # mov    %edx,%ecx
                0x0f, 0xa2,              # cpuid
                0x89, 0x07,              # mov    %eax,(%rdi)
                0x89, 0x5f, 0x04,        # mov    %ebx,0x4(%rdi)
                0x89, 0x4f, 0x08,        # mov    %ecx,0x8(%rdi)
                0x89, 0x57, 0x0c,        # mov    %edx,0xc(%rdi)
                0x5b,                    # pop    %rbx
                0xc3                     # retq
            ]
    else:
        # CDECL 32 bit
        # Three first call registers : Stack (%esp)
        # Volatile registers         : EAX, ECX, EDX
        opc = [
            0x53,                    # push   %ebx
            0x57,                    # push   %edi
            0x8b, 0x7c, 0x24, 0x0c,  # mov    0xc(%esp),%edi
            0x8b, 0x44, 0x24, 0x10,  # mov    0x10(%esp),%eax
            0x8b, 0x4c, 0x24, 0x14,  # mov    0x14(%esp),%ecx
            0x0f, 0xa2,              # cpuid
            0x89, 0x07,              # mov    %eax,(%edi)
            0x89, 0x5f, 0x04,        # mov    %ebx,0x4(%edi)
            0x89, 0x4f, 0x08,        # mov    %ecx,0x8(%edi)
            0x89, 0x57, 0x0c,        # mov    %edx,0xc(%edi)
            0x5f,                    # pop    %edi
            0x5b,                    # pop    %ebx
            0xc3                     # ret
        ]

    code_size = len(opc)
    code = (ctypes.c_ubyte * code_size)(*opc)

    if is_windows:
        # Allocate executable memory
        libc.VirtualAlloc.restype = ctypes.c_void_p
        libc.VirtualAlloc.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_ulong, ctypes.c_ulong]
        addr = libc.VirtualAlloc(None, code_size, 0x1000, 0x40)
        if not addr:
            raise MemoryError("Could not VirtualAlloc RWX memory")
    else:
        # Allocate memory
        libc.valloc.restype = ctypes.c_void_p
        libc.valloc.argtypes = [ctypes.c_size_t]
        addr = libc.valloc(code_size)
        if not addr:
            raise MemoryError("Could not valloc memory")

        # Make executable
        libc.mprotect.restype = ctypes.c_int
        libc.mprotect.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_int]
        if 0 != libc.mprotect(addr, code_size, 1 | 2 | 4):
            raise OSError("Failed to set RWX using mprotect")

    # Copy code to allocated executable memory. No need to flush instruction
    # cache for CPUID.
    ctypes.memmove(addr, code, code_size)

    # Create and yield callable
    result = CPUID_struct()
    func_type = ctypes.CFUNCTYPE(None, ctypes.POINTER(CPUID_struct), ctypes.c_uint32, ctypes.c_uint32)
    func_ptr = func_type(addr)

    def cpuid(eax, ecx=0):
        func_ptr(result, eax, ecx)
        return result.eax, result.ebx, result.ecx, result.edx

    yield cpuid

    # Free
    if is_windows:
        libc.VirtualFree.restype = ctypes.c_long
        libc.VirtualFree.argtypes = [ctypes.c_void_p, ctypes.c_size_t, ctypes.c_ulong]
        libc.VirtualFree(addr, 0, 0x8000)
    else:
        libc.free.restype = None
        libc.free.argtypes = [ctypes.c_void_p]
        libc.free(addr)


def cmd_cpuid(argv):
    with make_cpuid() as cpuid:
        headers = ["CPUID", "EAX", "EBX", "ECX", "EDX"]
        print(" ".join(header.ljust(8) for header in headers).rstrip())

        for eax in [0x0, 0x80000000]:
            highest, _, _, _ = cpuid(eax)
            for eax in range(eax, highest + 1):
                a, b, c, d = cpuid(eax)
                print("%08x %08x %08x %08x %08x" % (eax, a, b, c, d))


def main(argv):
    # Parse command line arguments
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--verbose", "-v", default=0, action="count", help="increase verbosity")
    parser.add_argument("--version", action="version", version="fishnet v{0}".format(__version__))

    g = parser.add_argument_group("configuration")
    g.add_argument("--auto-update", action="store_true", help="automatically install available updates on startup and at random intervals")
    g.add_argument("--conf", help="configuration file")
    g.add_argument("--no-conf", action="store_true", help="do not use a configuration file")
    g.add_argument("--key", "--apikey", "-k", help="fishnet api key")

    g = parser.add_argument_group("resources")
    g.add_argument("--cores", help="number of cores to use for engine processes (or auto for n - 1, or all for n)")
    g.add_argument("--memory", help="total memory (MB) to use for engine hashtables")

    g = parser.add_argument_group("advanced")
    g.add_argument("--endpoint", help="lichess http endpoint (default: %s)" % DEFAULT_ENDPOINT)
    g.add_argument("--engine-dir", help="engine working directory")
    g.add_argument("--stockfish-command", help="stockfish command (default: download latest precompiled Stockfish)")
    g.add_argument("--threads-per-process", type=int, help="hint for the number of threads to use per engine process (default: %d)" % DEFAULT_THREADS)
    g.add_argument("--user-backlog", type=str, help="prefer to run high-priority jobs only if older than this duration (for example 120s)")
    g.add_argument("--system-backlog", type=str, help="prefer to run low-priority jobs only if older than this duration (for example 2h)")
    g.add_argument("--fixed-backoff", action="store_true", default=None, help="only for developers: do not use exponential backoff")
    g.add_argument("--no-fixed-backoff", dest="fixed_backoff", action="store_false", default=None)
    g.add_argument("--setoption", "-o", nargs=2, action="append", default=[], metavar=("NAME", "VALUE"), help="only for developers: set a custom uci option")
    g.add_argument("--threads", type=int, dest="ignored_threads", help=argparse.SUPPRESS)  # bc

    commands = collections.OrderedDict([
        ("run", cmd_run),
        ("configure", cmd_configure),
        ("systemd", cmd_systemd),
        ("systemd-user", cmd_systemd),
        ("cpuid", cmd_cpuid),
    ])

    parser.add_argument("command", default="run", nargs="?", choices=commands.keys())

    try:
        import argcomplete
    except ImportError:
        pass
    else:
        argcomplete.autocomplete(parser)
    args = parser.parse_args(argv[1:])

    # Setup logging
    setup_logging(args.verbose,
                  sys.stderr if args.command in ["systemd", "systemd-user"] else sys.stdout)

    # Show intro
    if args.command not in ["systemd", "systemd-user", "cpuid"]:
        print(intro())
        sys.stdout.flush()

    # Run
    try:
        sys.exit(commands[args.command](args))
    except UpdateRequired:
        if args.auto_update:
            logging.info("\n\n### Updating ...\n")
            update_self()

        logging.error("Update required. Exiting (status 70)")
        return 70
    except ConfigError:
        logging.exception("Configuration error")
        return 78
    except (KeyboardInterrupt, Shutdown, ShutdownSoon):
        return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
