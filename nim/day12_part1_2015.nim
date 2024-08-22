import json

proc sumNumbers(j: JsonNode): int =
  case j.kind
  of JInt: return j.getInt()
  of JFloat: return int(j.getFloat())
  of JArray:
    for item in j.elems:
      result += sumNumbers(item)
  of JObject:
    for _, value in j.pairs:
      result += sumNumbers(value)
  else: return 0

let file = readFile("input.txt")
let jsonDoc = parseJson(file)
echo sumNumbers(jsonDoc)