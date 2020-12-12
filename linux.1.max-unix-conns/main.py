#!/usr/bin/env python3

import signal
import socket
import sys
import os
import tempfile
import time

uds_addr = './uds_socket'


def main():
    # man 5 proc
    fp = "/proc/sys/fs/file-max"
    with open(fp, "r") as f:
        exp_max = int(f.read())
        print("{} = {:,}".format(fp, exp_max))

    try:
        os.unlink(uds_addr)
    except OSError:
        if os.path.exists(uds_addr):
            raise

    pid = os.fork()
    if pid > 0:
        connect(pid)
    else:
        listen()


def listen():
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.bind(uds_addr)

    # Listen for incoming connections
    sock.listen(1)

    conns = []
    try:
        while True:
            connection, client_addr = sock.accept()
            conns.append(connection)
    except BaseException:
        print("Server connections: {:,}".format(len(conns),))


def _wait_for_socket():
    i = 0
    while not os.path.exists(uds_addr):
        time.sleep(0.1)
        i += 1
        if i >= 10:
            print("fail")
            sys.exit(1)

def connect(pid_server):
    _wait_for_socket()

    conns = []
    while True:
        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(uds_addr)
        except BaseException as e:
            print(e, file=sys.stderr)
            print("Client conns: {:,}".format(len(conns),))
            os.kill(pid_server, signal.SIGTERM)
            os.waitpid(pid_server, 0)
            sys.exit(1)
        else:
            conns.append(sock)


if __name__ == "__main__":
    main()

