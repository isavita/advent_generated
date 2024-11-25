
import "io" for File

class Rule {
    construct new(category, operator, num, workflowName) {
        _category = category
        _operator = operator
        _num = num
        _workflowName = workflowName
    }

    category { _category }
    operator { _operator }
    num { _num }
    workflowName { _workflowName }
}

class Solver {
    static solve(input) {
        var workflows = {}
        var parts = []

        var i = 0
        while (input[i] != "") {
            var workflow = parseWorkflow(input[i])
            workflows[workflow[0]] = workflow[1]
            i = i + 1
        }

        for (j in (i+1)...input.count) {
            parts.add(parsePart(input[j]))
        }

        var res = 0
        for (part in parts) {
            if (applyWorkflow(part, workflows, "in")) {
                res = res + part.values.reduce(0) { |sum, val| sum + val }
            }
        }

        return res
    }

    static parseWorkflow(line) {
        var idx = line.indexOf("{")
        var workflowName = line[0...idx]
        var rulesStr = line[idx+1...line.count-1].split(",")
        
        var rules = []
        for (ruleStr in rulesStr) {
            var colonIdx = ruleStr.indexOf(":")
            
            if (colonIdx == -1) {
                rules.add(Rule.new(null, null, null, ruleStr))
            } else {
                var category = ruleStr[0]
                var operator = ruleStr[1]
                var num = Num.fromString(ruleStr[2...colonIdx])
                var workflowName = ruleStr[colonIdx+1..-1]
                rules.add(Rule.new(category, operator, num, workflowName))
            }
        }

        return [workflowName, rules]
    }

    static parsePart(line) {
        var parts = line[1...-1].split(",")
        var partMap = {}
        for (part in parts) {
            var kv = part.split("=")
            partMap[kv[0]] = Num.fromString(kv[1])
        }
        return partMap
    }

    static applyWorkflow(part, workflows, workflowName) {
        if (workflowName == "A") return true
        if (workflowName == "R") return false

        for (rule in workflows[workflowName]) {
            var isValid = true
            
            if (rule.category != null) {
                var rating = part[rule.category]
                if (rule.operator == ">") {
                    isValid = rating > rule.num
                } else if (rule.operator == "<") {
                    isValid = rating < rule.num
                }
            }

            if (isValid) {
                return applyWorkflow(part, workflows, rule.workflowName)
            }
        }

        return false
    }
}

var input = File.read("input.txt").trim().split("\n")
System.print(Solver.solve(input))
