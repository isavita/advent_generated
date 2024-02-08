
def data = new File("input.txt").text.trim().split(",")
def original = data.collect { it as Integer }

for (int noun = 0; noun <= 99; noun++) {
    for (int verb = 0; verb <= 99; verb++) {
        def memory = original.clone()
        memory[1] = noun
        memory[2] = verb
        if (execute(memory) == 19690720) {
            println(100 * noun + verb)
            return
        }
    }
}

def execute(memory) {
    for (int i = 0; i < memory.size(); i += 4) {
        switch (memory[i]) {
            case 1:
                memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]]
                break
            case 2:
                memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]]
                break
            case 99:
                return memory[0]
        }
    }
    return memory[0]
}
