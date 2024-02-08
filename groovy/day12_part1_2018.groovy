
def initialState = ""
def rules = [:]

new File("input.txt").eachLine { line ->
    if (line.startsWith("initial state: ")) {
        initialState = line.substring(15)
    } else if (line.contains("=>")) {
        def parts = line.split(" => ")
        rules.put(parts[0], parts[1])
    }
}

def state = initialState
def offset = 0

for (int gen = 0; gen < 20; gen++) {
    def nextState = ""
    state = "....$state...."
    offset -= 2

    for (int i = 2; i < state.size() - 2; i++) {
        def sub = state.substring(i - 2, i + 3)
        nextState += rules.containsKey(sub) ? rules[sub] : "."
    }

    state = nextState
}

def sum = 0
for (int i = 0; i < state.size(); i++) {
    sum += (i + offset) * (state[i] == "#" ? 1 : 0)
}

println sum
