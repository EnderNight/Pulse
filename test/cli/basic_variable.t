  $ echo "let a = 34; let b = 35; a + b;" > test.pulse

  $ pulse exec test.pulse
  69

  $ rm test.pulse
