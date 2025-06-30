  $ echo "print 1/0;" > test.pulse

  $ pulse exec test.pulse
  Error: division by zero
  [1]

  $ rm test.pulse
