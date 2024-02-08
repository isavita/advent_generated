
def input = new File("input.txt").text.tokenize("\n")

int depth = input[0].split(" ")[1] as int
def target = input[1].split(" ")[1].tokenize(",").collect { it as int }

int riskLevel = 0
def erosionLevels = [:]
def typeMap = ["rocky": 0, "wet": 1, "narrow": 2]

for (int y = 0; y <= target[1]; y++) {
    for (int x = 0; x <= target[0]; x++) {
        int geologicIndex
        if ((x == 0 && y == 0) || (x == target[0] && y == target[1])) {
            geologicIndex = 0
        } else if (y == 0) {
            geologicIndex = x * 16807
        } else if (x == 0) {
            geologicIndex = y * 48271
        } else {
            geologicIndex = erosionLevels["${x-1},${y}"] * erosionLevels["${x},${y-1}"]
        }
        
        int erosionLevel = (geologicIndex + depth) % 20183
        int regionType = erosionLevel % 3
        riskLevel += regionType
        
        erosionLevels["${x},${y}"] = erosionLevel
    }
}

println riskLevel
