
import std/[strutils, sequtils, tables, os, math]

# --- Data Structures ---

type
  JobType* = enum
    Number, Add, Sub, Mul, Div, Eq # Added Eq for Part 2

  Job* = object
    case kind*: JobType
    of Number:
      num*: int64
    of Add, Sub, Mul, Div, Eq:
      op1*, op2*: string

  Jobs = Table[string, Job]
  Cache = Table[string, int64]
  DependsCache = Table[string, bool]

# --- Parsing ---

proc parseJob(s: string): Job =
  let parts = s.split(' ')
  if parts.len == 1:
    try:
      result = Job(kind: Number, num: parseInt(parts[0]).int64)
    except ValueError:
      raise newException(ValueError, "Invalid number format: " & s)
  elif parts.len == 3:
    let op1 = parts[0]
    let op2 = parts[2]
    case parts[1]
    of "+": result = Job(kind: Add, op1: op1, op2: op2)
    of "-": result = Job(kind: Sub, op1: op1, op2: op2)
    of "*": result = Job(kind: Mul, op1: op1, op2: op2)
    of "/": result = Job(kind: Div, op1: op1, op2: op2)
    of "=": result = Job(kind: Eq, op1: op1, op2: op2) # Part 2 root operation
    else:
      raise newException(ValueError, "Invalid operation: " & parts[1])
  else:
    raise newException(ValueError, "Invalid job format: " & s)

proc parseInput(filename: string): Jobs =
  result = initTable[string, Job]()
  for line in lines(filename):
    if line.len > 0:
      let parts = line.split(": ")
      if parts.len == 2:
        let monkeyName = parts[0]
        result[monkeyName] = parseJob(parts[1])
      else:
        raise newException(ValueError, "Invalid line format: " & line)

# --- Part 1: Evaluation ---

proc eval(monkey: string, jobs: Jobs, cache: var Cache): int64 =
  if monkey in cache:
    return cache[monkey]

  let job = jobs[monkey]
  case job.kind
  of Number:
    result = job.num
  of Add:
    result = eval(job.op1, jobs, cache) + eval(job.op2, jobs, cache)
  of Sub:
    result = eval(job.op1, jobs, cache) - eval(job.op2, jobs, cache)
  of Mul:
    result = eval(job.op1, jobs, cache) * eval(job.op2, jobs, cache)
  of Div:
    # Ensure integer division
    let val1 = eval(job.op1, jobs, cache)
    let val2 = eval(job.op2, jobs, cache)
    if val2 == 0:
      raise newException(ValueError, "Division by zero for monkey " & monkey)
    # Nim's `div` is integer division
    result = val1 div val2
  of Eq: # Eq is treated like Add for simple evaluation if needed, but not used in Part 1 logic directly
     raise newException(ValueError, "Equality encountered during simple evaluation")


  cache[monkey] = result

# --- Part 2: Solving for Humn ---

proc dependsOnHumn(monkey: string, jobs: Jobs, cache: var DependsCache): bool =
  if monkey == "humn":
    return true
  if monkey in cache:
    return cache[monkey]

  if monkey notin jobs: # Should not happen with valid input if humn is reachable
     return false

  let job = jobs[monkey]
  case job.kind
  of Number:
    result = false
  of Add, Sub, Mul, Div, Eq:
    result = dependsOnHumn(job.op1, jobs, cache) or dependsOnHumn(job.op2, jobs, cache)

  cache[monkey] = result

proc solve(monkey: string, target: int64, jobs: Jobs, evalCache: var Cache, dependsCache: var DependsCache): int64 =
  ## Recursively solves for the value `monkey` needs to produce to match `target`.
  ## Assumes `monkey` eventually depends on "humn".

  if monkey == "humn":
    return target # Base case: we found the value humn needs to be

  let job = jobs[monkey]
  # We should only be calling solve on operation monkeys in the humn path
  assert job.kind in {Add, Sub, Mul, Div}

  let op1Depends = dependsOnHumn(job.op1, jobs, dependsCache)
  let op2Depends = dependsOnHumn(job.op2, jobs, dependsCache)

  # Exactly one operand must depend on humn for the inversion to work simply
  assert op1Depends != op2Depends

  if op1Depends:
    # op1 depends on humn, op2 is constant
    let op2Value = eval(job.op2, jobs, evalCache) # Evaluate the constant branch
    case job.kind
    of Add: # target = new_target + op2Value => new_target = target - op2Value
      result = solve(job.op1, target - op2Value, jobs, evalCache, dependsCache)
    of Sub: # target = new_target - op2Value => new_target = target + op2Value
      result = solve(job.op1, target + op2Value, jobs, evalCache, dependsCache)
    of Mul: # target = new_target * op2Value => new_target = target / op2Value
      result = solve(job.op1, target div op2Value, jobs, evalCache, dependsCache)
    of Div: # target = new_target / op2Value => new_target = target * op2Value
      result = solve(job.op1, target * op2Value, jobs, evalCache, dependsCache)
    else: discard # Should not happen based on assertion

  else: # op2Depends must be true
    # op2 depends on humn, op1 is constant
    let op1Value = eval(job.op1, jobs, evalCache) # Evaluate the constant branch
    case job.kind
    of Add: # target = op1Value + new_target => new_target = target - op1Value
      result = solve(job.op2, target - op1Value, jobs, evalCache, dependsCache)
    of Sub: # target = op1Value - new_target => new_target = op1Value - target
      result = solve(job.op2, op1Value - target, jobs, evalCache, dependsCache)
    of Mul: # target = op1Value * new_target => new_target = target / op1Value
      result = solve(job.op2, target div op1Value, jobs, evalCache, dependsCache)
    of Div: # target = op1Value / new_target => new_target = op1Value / target
      result = solve(job.op2, op1Value div target, jobs, evalCache, dependsCache)
    else: discard # Should not happen based on assertion


# --- Main Execution ---

when isMainModule:
  let filename = "input.txt"
  if not fileExists(filename):
    echo "Error: Input file '", filename, "' not found."
    quit(1)

  var jobs = parseInput(filename)
  var evalCache = initTable[string, int64]()

  # --- Part 1 ---
  let rootValuePart1 = eval("root", jobs, evalCache)
  echo "Part 1: ", rootValuePart1

  # --- Part 2 ---
  # Reset caches and modify jobs for Part 2
  evalCache = initTable[string, int64]() # Clear eval cache as humn is now variable
  var dependsCache = initTable[string, bool]()

  # Modify root job
  var rootJob = jobs["root"]
  if rootJob.kind notin {Add, Sub, Mul, Div}:
      raise newException(ValueError, "Original root job is not a binary operation")
  rootJob.kind = Eq # Change root operation to equality
  jobs["root"] = rootJob

  # Remove humn's original number job if it exists (it's now the variable)
  # The evaluation logic handles this by checking dependsOnHumn
  if "humn" in jobs and jobs["humn"].kind == Number:
      discard # No need to explicitly remove, just don't use its number

  # Determine which side of root depends on humn and find the target value
  let rootOp1 = jobs["root"].op1
  let rootOp2 = jobs["root"].op2

  let rootOp1Depends = dependsOnHumn(rootOp1, jobs, dependsCache)
  let rootOp2Depends = dependsOnHumn(rootOp2, jobs, dependsCache)

  var humnValuePart2: int64 = -1 # Initialize with a sentinel value

  if rootOp1Depends == rootOp2Depends:
      raise newException(ValueError, "Both or neither side of root depends on humn - cannot solve directly.")

  if rootOp1Depends:
    # op1 depends on humn, op2 is the target value
    let targetValue = eval(rootOp2, jobs, evalCache)
    humnValuePart2 = solve(rootOp1, targetValue, jobs, evalCache, dependsCache)
  else:
    # op2 depends on humn, op1 is the target value
    let targetValue = eval(rootOp1, jobs, evalCache)
    humnValuePart2 = solve(rootOp2, targetValue, jobs, evalCache, dependsCache)

  echo "Part 2: ", humnValuePart2
