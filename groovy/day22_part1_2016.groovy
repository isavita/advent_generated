def input = new File("input.txt").readLines()

def viablePairs = 0

for (int i = 2; i < input.size(); i++) {
    def line = input[i].split("\\s+")
    def used = line[2].substring(0, line[2].length() - 1) as int
    def avail = line[3].substring(0, line[3].length() - 1) as int

    if (used != 0) {
        for (int j = 2; j < input.size(); j++) {
            if (i != j) {
                def otherLine = input[j].split("\\s+")
                def otherAvail = otherLine[3].substring(0, otherLine[3].length() - 1) as int

                if (used <= otherAvail) {
                    viablePairs++
                }
            }
        }
    }
}

println viablePairs