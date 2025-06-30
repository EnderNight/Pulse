  $ echo "print 72; print 101; print 108; print 108; print 111; print 32; print 87; print 111; print 114; print 108; print 100; print 33; print 10;" > test.pulse

  $ pulse compile test.pulse -o hello_world.pulsebyc

  $ pulse run hello_world.pulsebyc
  Hello World!

  $ rm test.pulse hello_world.pulsebyc
