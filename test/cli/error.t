  $ echo "34 35" > error.pulse

  $ pulse exec error.pulse
  error.pulse:1:4: expected semicolon.
  [1]

  $ rm error.pulse
