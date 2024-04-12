fs = require 'fs'

abs = (x) -> if x < 0 then -x else x

manhattan = (x) -> abs(x[0]) + abs(x[1]) + abs(x[2])

fs.readFile 'input.txt', 'utf8', (err, data) ->
  throw err if err

  particles = []
  lines = data.trim().split '\n'
  for line in lines
    parts = line.split ', '
    p = {p: [0, 0, 0], v: [0, 0, 0], a: [0, 0, 0]}
    for i in [0..2]
      coords = parts[i].slice(3, -1).split ','
      for j in [0..2]
        num = parseInt(coords[j], 10)
        switch i
          when 0 then p.p[j] = num
          when 1 then p.v[j] = num
          when 2 then p.a[j] = num
    particles.push p

  closestParticle = 0
  minAccel = Infinity
  minVelocity = Infinity
  minPosition = Infinity

  for i, particle of particles
    accel = manhattan particle.a
    velocity = manhattan particle.v
    position = manhattan particle.p

    if accel < minAccel or (accel == minAccel and velocity < minVelocity) or
       (accel == minAccel and velocity == minVelocity and position < minPosition)
      minAccel = accel
      minVelocity = velocity
      minPosition = position
      closestParticle = i

  console.log closestParticle