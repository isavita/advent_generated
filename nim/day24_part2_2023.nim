import strutils, math, sequtils

type Vec3 = object
  x, y, z: float

proc `+`(a, b: Vec3): Vec3 = Vec3(x: a.x + b.x, y: a.y + b.y, z: a.z + b.z)
proc `-`(a, b: Vec3): Vec3 = Vec3(x: a.x - b.x, y: a.y - b.y, z: a.z - b.z)
proc `*`(a: Vec3, s: float): Vec3 = Vec3(x: a.x * s, y: a.y * s, z: a.z * s)
proc `/`(a: Vec3, s: float): Vec3 = Vec3(x: a.x / s, y: a.y / s, z: a.z / s)

proc dot(a, b: Vec3): float = a.x * b.x + a.y * b.y + a.z * b.z
proc cross(a, b: Vec3): Vec3 =
  Vec3(x: a.y * b.z - a.z * b.y, y: a.z * b.x - a.x * b.z, z: a.x * b.y - a.y * b.x)

var h: seq[tuple[p, v: Vec3]]
let lines = readFile("input.txt").strip().splitLines()
for i in 0..2:
  let v = lines[i].replace("@", ",").split(",").mapIt(it.strip.parseFloat)
  h.add((Vec3(x: v[0], y: v[1], z: v[2]), Vec3(x: v[3], y: v[4], z: v[5])))

let
  r1p = h[1].p - h[0].p
  r1v = h[1].v - h[0].v
  r2p = h[2].p - h[0].p
  r2v = h[2].v - h[0].v
  pl2 = cross(r2p, r2v)
  pl1 = cross(r1p, r1v)
  t1 = -dot(r1p, pl2) / dot(r1v, pl2)
  t2 = -dot(r2p, pl1) / dot(r2v, pl1)
  rock1 = h[1].p + h[1].v * t1
  rock2 = h[2].p + h[2].v * t2
  vRock = (rock2 - rock1) / (t2 - t1)
  pRock = rock1 - vRock * t1

echo (pRock.x + pRock.y + pRock.z).round.int64