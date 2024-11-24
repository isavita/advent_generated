
import json, strutils, os

proc sumNumbers(data: JsonNode): int =
  if data.isNil:
    return 0
  
  case data.kind
  of JArray:
    for item in data:
      result += sumNumbers(item)
  of JObject:
    var hasRed = false
    for _, val in data:
      if val.kind == JString and val.getStr == "red":
        hasRed = true
        break
    
    if not hasRed:
      for _, val in data:
        result += sumNumbers(val)
  of JFloat, JInt:
    result = int(data.getFloat)
  else:
    discard

proc main() =
  let content = readFile("input.txt")
  let jsonData = parseJson(content)
  echo sumNumbers(jsonData)

main()
