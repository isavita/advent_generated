def input = new File("input.txt").readLines()

def lowPoints = []

for (int i = 0; i < input.size(); i++) {
    for (int j = 0; j < input[i].size(); j++) {
        def current = input[i][j] as int
        def up = i > 0 ? input[i-1][j] as int : null
        def down = i < input.size() - 1 ? input[i+1][j] as int : null
        def left = j > 0 ? input[i][j-1] as int : null
        def right = j < input[i].size() - 1 ? input[i][j+1] as int : null

        if ((up == null || current < up) &&
            (down == null || current < down) &&
            (left == null || current < left) &&
            (right == null || current < right)) {
            lowPoints.add(current)
        }
    }
}

def sum = lowPoints.sum() + lowPoints.size()

println sum