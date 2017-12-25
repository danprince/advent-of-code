# http://adventofcode.com/2017/day/20

import sequtils, part_one

proc `+`(a: Vector, b: Vector): Vector =
  return (x: a.x + b.x, y: a.y + b.y, z: a.z + b.z)

proc update(particle: Particle): Particle =
  let v = particle.a + particle.v
  let p = particle.p + v
  return (p: p, v: v, a: particle.a)

proc hasCollision(particle: Particle, particles: seq[Particle]): bool =
  var found_self = false

  for other in particles:
    if other.p == particle.p:
      if found_self:
        return true
      else:
        found_self = true

  return false

proc removeCollisions(particles: seq[Particle]): seq[Particle] =
  return filter(particles) do (particle: Particle) -> bool :
    not hasCollision(particle, particles)

proc solve(input: string) =
  var particles = parseInput(input)

  for i in 1..100:
    particles = map(particles, update)
    particles = removeCollisions(particles)

  echo particles.len
