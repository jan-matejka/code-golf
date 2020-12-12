#!/usr/bin/env sh

# fun stuff

# # get the current limits:
# # prlimit -p $$ -n
# RESOURCE DESCRIPTION              SOFT    HARD UNITS
# NOFILE   max number of open files 1024 1048576 files
#
# # increase soft limit to hard:
# # prlimit -p $$ --nofile=1048576:1048576

# # increase soft and hard over current hard
# # prlimit -p $$ --nofile=1048577:1048577
# prlimit: failed to set the NOFILE resource limit: Operation not permitted

set -e

# get hard limit
hard=$(prlimit --pid $$ -n --o HARD | tail -n1)

# set soft to hard, hard can not be increased anymore
# (at least not in this way over 1048576 on my system)
sudo prlimit -p $$ --nofile=$hard

printf "nofile limits set to $hard\nexpecting almost as much connections\n"
python3 main.py
