  $ echo "34 + 35;" > test.pulse

  $ pulse compile test.pulse

  $ pulse disasm a.pulsebyc
  Pulse v0.2.0
  Variable pool count: 0
  
  PUSH 34
  PUSH 35
  ADD
  HALT
  

  $ rm test.pulse
