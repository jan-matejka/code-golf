How many unix socket connections can be opened at once?
#######################################################

Example output:

::

   % ./main.sh
   nofile limits set to 1048576
   expecting almost as much connections
   /proc/sys/fs/file-max = 6,554,877
   Server connections: 1,048,571
   [Errno 24] Too many open files
   Client conns: 1,048,572
