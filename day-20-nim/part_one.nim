# http://adventofcode.com/2017/day/20

import strutils, sequtils, nre

type
  Vector* = tuple[x: int, y: int, z: int]
  Particle* = tuple[p: Vector, v: Vector, a: Vector]

proc parseVector(src: string): Vector =
  let parts = split(src, ",")
  let x = parts[0]
  let y = parts[1]
  let z = parts[2]
  return (x: parseInt(x), y: parseInt(y), z: parseInt(z))

proc parseParticle(src: string): Particle =
  let match = src.match(re"p=<(.*)>, v=<(.*)>, a=<(.*)>")
  let captures = match.get().captures
  let p = captures[0]
  let v = captures[1]
  let a = captures[2]
  return (p: parseVector(p), v: parseVector(v), a: parseVector(a))

proc parseInput*(src: string): seq[Particle] =
  let rows = splitLines(src)
  return map(rows, parseParticle)

proc magnitude*(vec: Vector): int =
  return abs(vec.x) + abs(vec.y) + abs(vec.z)

proc solve(input: string) =
  let particles = parseInput(input)
  let firstParticle = particles[0]
  var minAcceleration = magnitude(firstParticle.a)
  var minIndex = 0

  for i in 1 .. particles.len - 1:
    let particle = particles[i]
    let acceleration = magnitude(particle.a)
    if acceleration < minAcceleration:
      minAcceleration = acceleration
      minIndex = i

  echo minIndex
