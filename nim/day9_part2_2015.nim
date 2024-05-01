import sequtils
import strutils
import math

type
  City = string
  Distance = int
  Route = seq[City]

proc parseInput(filename: string): (seq[City], seq[(City, City, Distance)]) =
  var cities: seq[City] = @[]
  var distances: seq[(City, City, Distance)] = @[]
  for line in lines filename:
    let parts = line.split(" = ")
    let distance = parseInt(parts[1])
    let citiesParts = parts[0].split(" to ")
    let city1 = citiesParts[0].strip()
    let city2 = citiesParts[1].strip()
    if city1 notin cities:
      cities.add city1
    if city2 notin cities:
      cities.add city2
    distances.add (city1, city2, distance)
  return (cities, distances)

proc calculateRouteDistance(route: Route, distances: seq[(City, City, Distance)]): Distance =
  var distance: Distance = 0
  for i in 0..<route.len - 1:
    for (city1, city2, dist) in distances:
      if (city1 == route[i] and city2 == route[i + 1]) or (city1 == route[i + 1] and city2 == route[i]):
        distance += dist
        break
  return distance

proc permute[T](s: seq[T]): seq[seq[T]] =
  if s.len == 1:
    return @[s]
  result = @[]
  for i in 0..<s.len:
    let x = s[i]
    let remaining = s[0..<i] & s[i+1..<s.len]
    for p in permute(remaining):
      result.add @[x] & p

proc solve(filename: string) =
  let (cities, distances) = parseInput(filename)
  let routes = permute(cities)
  var shortestDistance: Distance = high(Distance)
  var longestDistance: Distance = low(Distance)
  for route in routes:
    let distance = calculateRouteDistance(route, distances)
    if distance < shortestDistance:
      shortestDistance = distance
    if distance > longestDistance:
      longestDistance = distance
  echo "Shortest distance: ", shortestDistance
  echo "Longest distance: ", longestDistance

when isMainModule:
  solve("input.txt")