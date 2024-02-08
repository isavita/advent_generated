def fabric = new int[1000][1000]
def count = 0

new File('input.txt').eachLine { line ->
    def parts = line.split(' ')
    def startX = parts[2].split(',')[0] as int
    def startY = parts[2].split(',')[1][0..-2] as int
    def width = parts[3].split('x')[0] as int
    def height = parts[3].split('x')[1] as int

    for (int i = startX; i < startX + width; i++) {
        for (int j = startY; j < startY + height; j++) {
            if (fabric[i][j] == 1) {
                count++
            }
            fabric[i][j]++
        }
    }
}

println count