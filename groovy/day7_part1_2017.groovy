def input = new File("input.txt").text.tokenize("\n")

def programs = [:]
def holdingPrograms = []

input.each {
    def parts = it.split(" -> ")
    def program = parts[0].split(" ")[0]
    def weight = parts[0].split(" ")[1].replaceAll("[\\(\\)]", "")
    programs[program] = weight.toInteger()
    
    if (parts.size() > 1) {
        holdingPrograms.addAll(parts[1].split(", "))
    }
}

def bottomProgram = programs.find { program, weight ->
    !holdingPrograms.contains(program)
}

println bottomProgram.getKey()