
def main() {
    int register5 = 0
    Set<Integer> seen = new HashSet<>()
    int lastUnique = 0

    while (true) {
        int register3 = register5 | 65536
        register5 = 7586220

        while (true) {
            int register1 = register3 & 255
            register5 = (((register5 + register1) & 16777215) * 65899) & 16777215

            if (register3 < 256) {
                if (seen.contains(register5)) {
                    println "Part Two Answer: ${lastUnique}"
                    return
                }
                seen.add(register5)
                lastUnique = register5
                if (seen.size() == 1) {
                }
                break
            } else {
                register3 /= 256
            }
        }
    }
}

main()
