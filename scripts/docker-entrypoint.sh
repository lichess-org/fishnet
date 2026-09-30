#!/bin/bash -e

args=("--no-conf")

if [ -n "$STATS_FILE" ]; then args+=("--stats-file" "$STATS_FILE"); else args+=("--no-stats-file"); fi
if [ -n "$KEY" ]; then args+=("--key" "$KEY"); fi
if [ -n "$KEY_FILE" ]; then args+=("--key-file" "$KEY_FILE"); fi
if [ -n "$CORES" ]; then args+=("--cores" "$CORES"); fi
if [ -n "$ENDPOINT" ]; then args+=("--endpoint" "$ENDPOINT"); fi
if [ -n "$USER_BACKLOG" ]; then args+=("--user-backlog" "$USER_BACKLOG"); fi
if [ -n "$SYSTEM_BACKLOG" ]; then args+=("--system-backlog" "$SYSTEM_BACKLOG"); fi
if [ -n "$MAX_BACKOFF" ]; then args+=("--max-backoff" "$MAX_BACKOFF"); fi
if [ -n "$CPU_PRIORITY" ]; then args+=("--cpu-priority" "$CPU_PRIORITY"); fi

: "${NICENESS:=0}"
exec nice -n "$NICENESS" /fishnet "${args[@]}"
