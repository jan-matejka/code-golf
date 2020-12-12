Connection Counter
==================

  $ $EXE &
  $ p=$!
  $ echo $p
  [0-9]+ (re)
  $ trap "kill $p" EXIT
  $ sleep 1

  $ ps -o pid= -p $p
  \s*\d+ (re)

  $ echo | nc localhost 8888
  You are: 0.
  $ echo | nc localhost 8888
  You are: 1.
