cmp
===

  $ echo foo > f1
  $ echo foo > f2
  $ $EXE f1 f2

  $ echo a >> f2
  $ $EXE f1 f2
  [1]

  $ echo b >> f1
  $ $EXE f1 f2
  [1]
