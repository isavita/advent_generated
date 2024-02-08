
def input = new File("input.txt").readLines()

def screen = new boolean[6][50]

input.each {
    def parts = it.split()
    switch (parts[0]) {
        case "rect":
            def size = parts[1].split("x").collect { it as int }
            for (int i = 0; i < size[1]; i++) {
                for (int j = 0; j < size[0]; j++) {
                    screen[i][j] = true
                }
            }
            break
        case "rotate":
            if (parts[1] == "row") {
                def row = parts[2].split("=")[1] as int
                def amount = parts[4] as int
                for (int i = 0; i < amount; i++) {
                    boolean temp = screen[row][49]
                    for (int j = 48; j >= 0; j--) {
                        screen[row][j + 1] = screen[row][j]
                    }
                    screen[row][0] = temp
                }
            } else if (parts[1] == "column") {
                def col = parts[2].split("=")[1] as int
                def amount = parts[4] as int
                for (int i = 0; i < amount; i++) {
                    boolean temp = screen[5][col]
                    for (int j = 4; j >= 0; j--) {
                        screen[j + 1][col] = screen[j][col]
                    }
                    screen[0][col] = temp
                }
            }
            break
    }
}

def count = 0
for (int i = 0; i < 6; i++) {
    for (int j = 0; j < 50; j++) {
        if (screen[i][j]) {
            count++
        }
    }
}

println count
