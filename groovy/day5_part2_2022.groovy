
def solve() {
    def parts = new File("input.txt").text.trim().split("\n\n")
    def input = parts[0].split("\n")
    def stacks = (0..(input[0].size() - 1) / 4).collect { [] }
    input.each { line ->
        line.bytes.eachWithIndex { b, i ->
            if (b >= 'A' && b <= 'Z') {
                stacks[(i - 1) / 4] << (char)b
            }
        }
    }
    def steps = parts[1].split("\n")
    println move(stacks, steps)
}

def move(st, steps) {
    def stacks = st.collect { it.reverse() }
    steps.each { step ->
        def (n, from, to) = step.split(" ").findAll { it =~ /\d+/ }.collect { it.toInteger() }
        from--
        to--
        stacks[to].addAll(stacks[from].takeRight(n))
        stacks[from] = stacks[from].dropRight(n)
    }
    stacks.collect { it.last() }.join()
}

solve()
