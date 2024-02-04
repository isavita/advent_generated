
FlipFlop = '%'
Conjunction = '&'

class Module:
    def __init__(self, name, prefix, destinations):
        self.name = name
        self.prefix = prefix
        self.destinations = destinations
        self.state = False
        self.memory = {}

class Pulse:
    def __init__(self, value, fromName, toName):
        self.value = value
        self.fromName = fromName
        self.toName = toName

Low = 0
High = 1

def parseInput(input):
    prefixes = [FlipFlop, Conjunction]
    modules = {}

    for line in input:
        parts = line.split(" -> ")

        module = Module("", None, [])
        isPrefix = False
        for prefix in prefixes:
            if parts[0][0] == prefix:
                module.prefix = prefix
                module.name = parts[0][1:]
                isPrefix = True
                continue
        if not isPrefix:
            module.name = parts[0]
        module.destinations = parts[1].split(", ")
        module.memory = {}

        modules[module.name] = module

    for module in modules.values():
        for destName in module.destinations:
            if destName in modules and modules[destName].prefix == Conjunction:
                modules[destName].memory[module.name] = Low

    return modules

def pushButton(modules, startPulse, numCycle):
    cntLow = 0
    cntHigh = 0
    pulseQueue = []

    for _ in range(numCycle):
        pulseQueue.append(startPulse)

        while pulseQueue:
            pulse = pulseQueue.pop(0)

            if pulse.value == Low:
                cntLow += 1
            else:
                cntHigh += 1

            if pulse.toName not in modules:
                continue

            module = modules[pulse.toName]
            if module.prefix == FlipFlop:
                if pulse.value == Low:
                    module.state = not module.state
                    if module.state:
                        newPulseValue = High
                    else:
                        newPulseValue = Low
                else:
                    continue
            elif module.prefix == Conjunction:
                module.memory[pulse.fromName] = pulse.value
                isHighForAll = all(value == High for value in module.memory.values())
                if isHighForAll:
                    newPulseValue = Low
                else:
                    newPulseValue = High
            else:
                newPulseValue = pulse.value

            for destName in module.destinations:
                newPulse = Pulse(newPulseValue, pulse.toName, destName)
                pulseQueue.append(newPulse)

    return cntLow, cntHigh

def solve(input):
    startPulse = Pulse(Low, "button", "broadcaster")
    numCycle = 1000

    modules = parseInput(input)

    cntLow, cntHigh = pushButton(modules, startPulse, numCycle)

    return cntLow * cntHigh

def readFile(fileName):
    with open(fileName, 'r') as file:
        return file.read().splitlines()

if __name__ == "__main__":
    input = readFile("input.txt")
    print(solve(input))
