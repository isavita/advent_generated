
import std/[strutils, sequtils, algorithm]

type
  Group = ref object
    army, units, hp, damage, initiative: int
    attackType: string
    weaknesses, immunities: seq[string]
    target, targetedBy: Group

proc ep(g: Group): int = g.units * g.damage

proc dmg(att, dfn: Group): int =
  if dfn.immunities.contains(att.attackType): return 0
  result = att.ep
  if dfn.weaknesses.contains(att.attackType): result *= 2

proc parse(): seq[Group] =
  var 
    res: seq[Group]
    army = 0
  for raw in readFile("input.txt").splitLines():
    let line = raw.strip()
    if line == "": continue
    if line.endsWith(":"):
      army = if line.startsWith("Immune"): 1 else: 2
      continue
    let 
      w = line.split(" ")
      g = Group(army: army, units: w[0].parseInt, hp: w[4].parseInt, initiative: w[^1].parseInt)
      di = w.find("damage")
    g.damage = w[di-2].parseInt
    g.attackType = w[di-1]
    if "(" in line:
      let m = line[line.find('(')+1 .. line.find(')')-1]
      for p in m.split("; "):
        if p.startsWith("weak to "): g.weaknesses = p[8..^1].split(", ")
        elif p.startsWith("immune to "): g.immunities = p[10..^1].split(", ")
    res.add(g)
  return res

var groups = parse()
while true:
  var active = groups.filterIt(it.units > 0)
  if active.len == 0 or active.allIt(it.army == active[0].army): break
  active.sort do (a, b: Group) -> int:
    if a.ep != b.ep: cmp(b.ep, a.ep) else: cmp(b.initiative, a.initiative)
  for g in groups:
    g.target = nil
    g.targetedBy = nil
  for att in active:
    var 
      best: Group = nil
      mx = 0
    for dfn in active:
      if dfn.army == att.army or dfn.targetedBy != nil: continue
      let d = att.dmg(dfn)
      if d == 0: continue
      if best == nil or d > mx:
        best = dfn
        mx = d
      elif d == mx:
        if dfn.ep > best.ep: best = dfn
        elif dfn.ep == best.ep:
          if dfn.initiative > best.initiative: best = dfn
    if best != nil:
      att.target = best
      best.targetedBy = att
  var attackers = active.filterIt(it.target != nil)
  attackers.sort do (a, b: Group) -> int:
    cmp(b.initiative, a.initiative)
  var totalKilled = 0
  for att in attackers:
    if att.units > 0:
      let k = min(att.target.units, att.dmg(att.target) div att.target.hp)
      att.target.units -= k
      totalKilled += k
  if totalKilled == 0: break

var s = 0
for g in groups:
  if g.units > 0: s += g.units
echo s

