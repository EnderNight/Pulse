  $ echo "34 + 35" > test.pulse

  $ pulse compile test.pulse

  $ pulse disasm a.pulsebyc
  PUSH 35
  PUSH 34
  ADD
  HALT

  $ rm test.pulse
