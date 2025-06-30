  $ echo "10 + b;" > test.pulse

  $ pulse exec test.pulse
  test.pulse:1:6: undeclared variable 'b'.
  [1]

  $ rm test.pulse
