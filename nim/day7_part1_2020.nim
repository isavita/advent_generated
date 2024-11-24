
import std/[strutils, tables, sets]

proc countCanContain(target: string, contains: Table[string, seq[string]]): int =
  var seen = initHashSet[string]()
  
  proc dfs(bag: string) =
    for outer in contains.getOrDefault(bag):
      if outer notin seen:
        seen.incl(outer)
        dfs(outer)
  
  dfs(target)
  return seen.len

proc main() =
  var contains = initTable[string, seq[string]]()
  
  for line in lines("input.txt"):
    let parts = line.split(" bags contain ")
    let container = parts[0]
    
    if parts[1] == "no other bags.":
      continue
    
    for bag in parts[1].split(", "):
      let bagName = bag.splitWhitespace()[1..2].join(" ")
      contains.mgetOrPut(bagName, @[]).add(container)
  
  echo countCanContain("shiny gold", contains)

main()
