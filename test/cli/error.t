  $ echo "34 35" > error.pulse

  $ pulse exec error.pulse
  error.pulse:1:4: Unexpected token. Expecting end of file

  $ rm error.pulse
