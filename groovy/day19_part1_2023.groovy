
def parseWorkflow(line) {
    def idx = line.indexOf('{')
    def workflowName = line[0..<idx]
    def rules = []
    def rulesStr = line[idx + 1..<line.length() - 1].split(',')
    for (def ruleStr : rulesStr) {
        def rule = [:]
        idx = ruleStr.indexOf(':')
        if (idx == -1) {
            rule.WorkflowName = ruleStr
        } else {
            rule.Category = ruleStr[0] as char
            rule.Operator = ruleStr[1] as char
            rule.Num = ruleStr[2..<idx].toInteger()
            rule.WorkflowName = ruleStr[idx + 1..-1]
        }
        rules << rule
    }
    return [workflowName, rules]
}

def parsePart(line) {
    def part = [:]
    def values = line[1..<line.length() - 1].split(',')
    for (def value : values) {
        def parts = value.split('=')
        part[parts[0] as char] = parts[1].toInteger()
    }
    return part
}

def parseInput(input) {
    def workflows = [:]
    def parts = []
    def i = 0
    while (input[i] != "") {
        def parsedWorkflow = parseWorkflow(input[i])
        workflows[parsedWorkflow[0]] = parsedWorkflow[1]
        i++
    }
    for (i = i + 1; i < input.size(); i++) {
        parts << parsePart(input[i])
    }
    return [workflows, parts]
}

def applyWorkflow(part, workflows, workflowName) {
    if (workflowName == "A") return true
    if (workflowName == "R") return false
    for (def rule : workflows[workflowName]) {
        def rating = part[rule.Category]
        def isValid = true
        switch (rule.Operator) {
            case '>':
                isValid = rating > rule.Num
                break
            case '<':
                isValid = rating < rule.Num
                break
            default:
                isValid = true
        }
        if (isValid) return applyWorkflow(part, workflows, rule.WorkflowName)
    }
    return false
}

def solve(input) {
    def startWorkflow = "in"
    def parsed = parseInput(input)
    def workflows = parsed[0]
    def parts = parsed[1]
    def res = 0
    for (def part : parts) {
        if (applyWorkflow(part, workflows, startWorkflow)) {
            res += part.values().sum()
        }
    }
    return res
}

def input = new File("input.txt").readLines()
println solve(input)
