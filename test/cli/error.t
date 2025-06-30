  $ echo "34 35" > error.pulse

  $ pulse exec error.pulse
  error.pulse:1:1: expected 'let' or 'print'.
  [1]

  $ rm error.pulse
