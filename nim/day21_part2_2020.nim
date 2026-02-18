
import tables, sets, strutils, algorithm

var allergenMap = initTable[string, HashSet[string]]()
var solved = initTable[string, string]()

for line in lines("input.txt"):
  if line.strip == "": continue
  let parts = line.split(" (contains ")
  let ingredients = parts[0].split(' ').toHashSet()
  if parts.len > 1:
    let allergens = parts[1][0 .. ^2].split(", ")
    for a in allergens:
      if a in allergenMap:
        allergenMap[a] = allergenMap[a].intersection(ingredients)
      else:
        allergenMap[a] = ingredients

while allergenMap.len > 0:
  var foundA, foundI: string
  for a, ings in allergenMap:
    if ings.len == 1:
      foundA = a
      for i in ings: foundI = i
      break
  solved[foundA] = foundI
  allergenMap.del(foundA)
  for a in allergenMap.keys:
    allergenMap[a].excl(foundI)

var keys = newSeq[string]()
for k in solved.keys: keys.add(k)
keys.sort()

var res = newSeq[string]()
for k in keys: res.add(solved[k])
echo res.join(",")
