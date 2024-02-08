
def file = new File("input.txt")
def input = file.text.trim()

def nice = 0

def passesRule1 = { line ->
    for (int i = 0; i < line.size() - 2; i++) {
        def toMatch = line.substring(i, i + 2)
        for (int j = i + 2; j < line.size() - 1; j++) {
            if (line.substring(j, j + 2) == toMatch) {
                return true
            }
        }
    }
    return false
}

input.split("\n").each { line ->
    def rule1 = passesRule1(line)

    def rule2 = false
    for (int i = 0; i < line.size() - 2; i++) {
        if (line[i] == line[i + 2]) {
            rule2 = true
            break
        }
    }
    
    if (rule1 && rule2) {
        nice++
    }
}

println nice
