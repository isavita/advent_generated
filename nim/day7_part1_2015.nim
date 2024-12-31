
import std/[strutils, tables, re]

proc someAssemblyRequired(input: string): int =
  var wireToRule: Table[string, string] = initTable[string, string]()

  for inst in input.splitLines:
    let parts = inst.split(" -> ")
    wireToRule[parts[1]] = parts[0]

  var memo: Table[string, int] = initTable[string, int]()

  proc memoDFS(graph: Table[string, string], entry: string): int =
    if memo.hasKey(entry):
      return memo[entry]

    if match(entry, re"[0-9]"):
      return parseInt(entry)

    let sourceRule = graph[entry]
    let parts = sourceRule.split(" ")

    var result: int
    case parts.len
    of 1:
      result = memoDFS(graph, parts[0])
    else:
      case parts[0]
      of "NOT":
        let start = memoDFS(graph, parts[1])
        result = (0xFFFF) xor start
      else:
        case parts[1]
        of "AND":
          result = memoDFS(graph, parts[0]) and memoDFS(graph, parts[2])
        of "OR":
          result = memoDFS(graph, parts[0]) or memoDFS(graph, parts[2])
        of "LSHIFT":
          result = memoDFS(graph, parts[0]) shl memoDFS(graph, parts[2])
        of "RSHIFT":
          result = memoDFS(graph, parts[0]) shr memoDFS(graph, parts[2])
        else:
          discard

    memo[entry] = result
    return result

  return memoDFS(wireToRule, "a")

let input = readFile("input.txt").strip()
echo someAssemblyRequired(input)
