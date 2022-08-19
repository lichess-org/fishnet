#!/bin/bash -e

args=("--no-conf")

if [ -n "$KEY" ]; then args+=("--key" "$KEY"); fi
if [ -n "$KEY_FILE" ]; then args+=("--key-file" "$KEY_FILE"); fi
if [ -n "$CORES" ]; then args+=("--cores" "$CORES"); fi
if [ -n "$ENDPOINT" ]; then args+=("--endpoint" "$ENDPOINT"); fi
if [ -n "$USER_BACKLOG" ]; then args+=("--user-backlog" "$USER_BACKLOG"); fi
if [ -n "$SYSTEM_BACKLOG" ]; then args+=("--system-backlog" "$SYSTEM_BACKLOG"); fi
if [ -n "$MAX_BACKOFF" ]; then args+=("--max-backoff" "$MAX_BACKOFF"); fi

exec /fishnet "${args[@]}"
