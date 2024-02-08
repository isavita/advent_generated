
def dfs(name, programs) {
    def program = programs[name]
    def totalWeight = program.Weight

    def weights = [:]
    program.Holds.each { child ->
        def (weight, balanced) = dfs(child, programs)
        if (!balanced) {
            return [0, false]
        }
        totalWeight += weight
        weights[weight] = weights.get(weight, 0) + 1
    }

    weights.each { w1, c1 ->
        weights.each { w2, c2 ->
            if (w1 != w2 && c1 < c2) {
                def unbalancedProgram = ""
                program.Holds.each { child ->
                    def (childWeight, _) = dfs(child, programs)
                    if (childWeight == w1) {
                        unbalancedProgram = child
                        return
                    }
                }
                println(programs[unbalancedProgram].Weight + (w2 - w1))
                return [0, false]
            }
        }
    }

    return [totalWeight, true]
}

def programs = [:]

def lines = new File("input.txt").readLines()

def re = ~/[a-z]+|\d+/

lines.each { line ->
    def matches = (line =~ re).collect { it }

    def name = matches[0]
    def weight = matches[1] as Integer

    def program = [Weight: weight]
    if (matches.size() > 2) {
        program.Holds = matches[2..-1]
    }
    programs[name] = program
}

def root = "dtacyn" // Replace this with the root found in Part One

dfs(root, programs)
