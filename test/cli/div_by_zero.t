  $ echo "1/0" > test.pulse

  $ pulse exec test.pulse
  Error: division by zero

  $ rm test.pulse
