#!/usr/bin/env bash

printf "DataDirectory .tor-data\n"
for port in {9070..9080}; do
  printf "%s\n" "SOCKSPort 127.0.0.1:$port IsolateDestAddr"
done
