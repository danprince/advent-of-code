import std/times

template time *(body: untyped): untyped =
  ## Julia-inspired time template, which times a block of code and prints
  ## the duration in seconds.
  block:
    let t1 = cpuTime()
    body
    let t2 = cpuTime()
    let s = (t2 - t1)
    echo $s & " seconds"