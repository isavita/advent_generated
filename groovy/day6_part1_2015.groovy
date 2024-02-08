def grid = new boolean[1000][1000]

new File('input.txt').eachLine { line ->
    def parts = line.split()
    def action = parts[0]
    def start = parts[parts.length - 3].split(',').collect { it.toInteger() }
    def end = parts[parts.length - 1].split(',').collect { it.toInteger() }

    for (int i = start[0]; i <= end[0]; i++) {
        for (int j = start[1]; j <= end[1]; j++) {
            switch (action) {
                case 'turn':
                    if (parts[1] == 'on') {
                        grid[i][j] = true
                    } else {
                        grid[i][j] = false
                    }
                    break
                case 'toggle':
                    grid[i][j] = !grid[i][j]
                    break
            }
        }
    }
}

def count = 0
for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 1000; j++) {
        if (grid[i][j]) {
            count++
        }
    }
}

println count