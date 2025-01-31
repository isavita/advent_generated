
local function readFile(fileName)
  local file = io.open(fileName, "r")
  if not file then
    error("File not found")
  end
  local lines = {}
  for line in file:lines() do
    table.insert(lines, line)
  end
  file:close()
  return lines
end

local function parseInput(input)
  local prefixes = { "%", "&" }
  local modules = {}
  for _, line in ipairs(input) do
    local parts = {}
    for part in string.gmatch(line, "([^%->%, ]+)") do
      table.insert(parts, part)
    end
    local module = {
      name = "",
      prefix = "",
      destinations = {},
      state = false,
      memory = {}
    }
    local isPrefix = false
    for _, prefix in ipairs(prefixes) do
      if parts[1]:sub(1, 1) == prefix then
        module.prefix = prefix
        module.name = parts[1]:sub(2)
        isPrefix = true
        break
      end
    end
    if not isPrefix then
      module.name = parts[1]
    end
    for i = 2, #parts do
      table.insert(module.destinations, parts[i])
    end
    modules[module.name] = module
  end
  for _, module in pairs(modules) do
    for _, destName in ipairs(module.destinations) do
      local destModule = modules[destName]
      if destModule and destModule.prefix == "&" then
        destModule.memory[module.name] = 0
      end
    end
  end
  return modules
end

local function pushButton(modules, startPulse, numCycle)
  local cntLow = 0
  local cntHigh = 0
  for _ = 1, numCycle do
    local pulseQueue = { startPulse }
    local head = 1
    while head <= #pulseQueue do
      local pulse = pulseQueue[head]
      head = head + 1
      if pulse.value == 0 then
        cntLow = cntLow + 1
      else
        cntHigh = cntHigh + 1
      end
      local module = modules[pulse.toName]
      if not module then
        goto continue_pulse
      end
      local newPulseValue
      if module.prefix == "%" then
        if pulse.value == 0 then
          module.state = not module.state
          if module.state then
            newPulseValue = 1
          else
            newPulseValue = 0
          end
        else
          goto continue_pulse
        end
      elseif module.prefix == "&" then
        module.memory[pulse.fromName] = pulse.value
        local isHighForAll = true
        for _, value in pairs(module.memory) do
          if value == 0 then
            isHighForAll = false
            break
          end
        end
        if isHighForAll then
          newPulseValue = 0
        else
          newPulseValue = 1
        end
      else
        newPulseValue = pulse.value
      end
      for _, destName in ipairs(module.destinations) do
        table.insert(pulseQueue, {
          value = newPulseValue,
          fromName = pulse.toName,
          toName = destName
        })
      end
      ::continue_pulse::
    end
  end
  return cntLow, cntHigh
end

local function solve(input)
  local startPulse = {
    value = 0,
    fromName = "button",
    toName = "broadcaster"
  }
  local numCycle = 1000
  local modules = parseInput(input)
  local cntLow, cntHigh = pushButton(modules, startPulse, numCycle)
  return cntLow * cntHigh
end

local input = readFile("input.txt")
print(solve(input))
