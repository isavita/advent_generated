
class Workflow {
    String name
    List<Rule> rules

    Workflow(String name, List<Rule> rules) {
        this.name = name
        this.rules = rules
    }

    String process(Map<String, Integer> part) {
        for (Rule rule : rules) {
            String result = rule.apply(part)
            if (result != null) {
                return result
            }
        }
        return null // Should not happen based on problem description
    }

     static Workflow parse(String line) {
        def name = line.substring(0, line.indexOf('{'))
        def rulesStr = line.substring(line.indexOf('{') + 1, line.indexOf('}'))
        def rules = rulesStr.split(',').collect { Rule.parse(it) }
        return new Workflow(name, rules)
    }
}

class Rule {
    String condition
    String destination

    Rule(String condition, String destination) {
        this.condition = condition
        this.destination = destination
    }

    String apply(Map<String, Integer> part) {
        if (condition == null) {
            return destination
        }

        def category = condition[0]
        def operator = condition[1]
        def value = Integer.parseInt(condition.substring(2))

        def partValue = part.get(category)

        if (operator == '<') {
            if (partValue < value) {
                return destination
            }
        } else if (operator == '>') {
            if (partValue > value) {
                return destination
            }
        }
        return null
    }

    static Rule parse(String ruleStr) {
        if (!ruleStr.contains(':')) {
            return new Rule(null, ruleStr)
        }
        def parts = ruleStr.split(':')
        return new Rule(parts[0], parts[1])
    }
}

class Part1 {
    static long solve(List<String> lines) {
        def workflows = [:]
        def parts = []
        def parsingWorkflows = true

        for (line in lines) {
            if (line.isEmpty()) {
                parsingWorkflows = false
                continue
            }

            if (parsingWorkflows) {
                def workflow = Workflow.parse(line)
                workflows.put(workflow.name, workflow)
            } else {
                def part = [:]
                line.substring(1, line.length() - 1).split(',').each {
                    def (key, value) = it.split('=')
                    part.put(key, Integer.parseInt(value))
                }
                parts.add(part)
            }
        }

        long totalRating = 0
        for (part in parts) {
            def workflowName = "in"
            while (workflowName != "A" && workflowName != "R") {
                workflowName = workflows.get(workflowName).process(part)
            }
            if (workflowName == "A") {
                totalRating += part.values().sum()
            }
        }
        return totalRating
    }
}


class Part2 {

    static long solve(List<String> lines) {
        def workflows = [:]
        for (line in lines) {
            if (line.isEmpty()) break

            def workflow = Workflow.parse(line)
            workflows.put(workflow.name, workflow)
        }

        return countAcceptedCombinations(workflows, "in",
                ['x': 1..4000, 'm': 1..4000, 'a': 1..4000, 's': 1..4000])
    }


    static long countAcceptedCombinations(Map<String, Workflow> workflows, String workflowName, Map<String, Range> ranges) {
       if (workflowName == "R") return 0
       if (workflowName == "A") {
           return ranges.values().collect { it.size() }.inject(1L) { acc, val -> acc * val }
       }

        long total = 0
        def workflow = workflows.get(workflowName)
        def currentRanges = ranges.clone()

        for (rule in workflow.rules) {
            if (rule.condition == null) {
                total += countAcceptedCombinations(workflows, rule.destination, currentRanges)
            } else {
                def category = rule.condition[0]
                def operator = rule.condition[1]
                def value = Integer.parseInt(rule.condition.substring(2))

                def range = currentRanges.get(category)

                if (operator == '<') {
                   def trueRange = range.from..<value
                    def falseRange = value..range.to
                    
                    if(!trueRange.isEmpty()) {
                        def newRanges = currentRanges.clone()
                        newRanges.put(category, trueRange)
                        total += countAcceptedCombinations(workflows, rule.destination, newRanges)
                    }
                     if (!falseRange.isEmpty()) {
                        currentRanges.put(category, falseRange)
                    }
                } else { // operator == '>'
                   def trueRange = (value + 1)..range.to
                    def falseRange = range.from..value
                    
                    if (!trueRange.isEmpty()) {
                        def newRanges = currentRanges.clone()
                        newRanges.put(category, trueRange)
                        total += countAcceptedCombinations(workflows, rule.destination, newRanges)
                    }
                    if (!falseRange.isEmpty()) {
                         currentRanges.put(category, falseRange)
                    }
                }

            }

        }
        return total
    }
}


static void main(String[] args) {
    def inputFile = new File("input.txt")
    def lines = inputFile.readLines()
     println "Part 1: ${Part1.solve(lines)}"
     println "Part 2: ${Part2.solve(lines)}"
}
