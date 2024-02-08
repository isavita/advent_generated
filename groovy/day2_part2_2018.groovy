
def lines = new File("input.txt").readLines()

for (int i = 0; i < lines.size() - 1; i++) {
    for (int j = i + 1; j < lines.size(); j++) {
        def diff = 0
        for (int k = 0; k < lines[i].size(); k++) {
            if (lines[i][k] != lines[j][k]) {
                diff++
                if (diff > 1) {
                    break
                }
            }
        }
        if (diff == 1) {
            def common = ""
            for (int k = 0; k < lines[i].size(); k++) {
                if (lines[i][k] == lines[j][k]) {
                    common += lines[i][k]
                }
            }
            println common
            return
        }
    }
}
