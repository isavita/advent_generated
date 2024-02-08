
def parseLine(line) {
    def parts = line.split(" = ")
    def head = parts[0]
    def childrenTrim = parts[1].replaceAll("[()]", "")
    def childrenParts = childrenTrim.split(", ")
    return [head, [childrenParts[0], childrenParts[1]]]
}

def gcd(a, b) {
    while (b != 0) {
        def temp = a
        a = b
        b = temp % b
    }
    return a
}

def lcm(a, b) {
    return (a * b) / gcd(a, b)
}

def lcmList(nums) {
    if (nums.size() == 0) {
        return 0
    }
    
    def res = nums[0]
    for (int i = 1; i < nums.size(); i++) {
        res = lcm(res, nums[i])
    }
    
    return res
}

def solve(input) {
    def instructions = input[0]
    def nodes = [:]
    for (int i = 2; i < input.size(); i++) {
        def parsedLine = parseLine(input[i])
        nodes[parsedLine[0]] = parsedLine[1]
    }
    
    def starts = []
    nodes.each { node ->
        if (node.key.endsWith("A")) {
            starts.add(node.key)
        }
    }
    
    def steps = new int[starts.size()]
    def instructionsLength = instructions.size()
    for (int i = 0; i < starts.size(); i++) {
        def element = starts[i]
        while (element[-1] != 'Z') {
            def instruction = instructions[steps[i] % instructionsLength]
            if (instruction == 'L') {
                element = nodes[element][0]
            } else {
                element = nodes[element][1]
            }
            steps[i]++
        }
    }
    
    return lcmList(steps)
}

def input = new File("input.txt").text.readLines()
println solve(input)
