def input = new File("input.txt").text.trim()

def countSafeTiles(input, rows) {
    def safeCount = 0
    def currentRow = input

    for (int i = 0; i < rows; i++) {
        safeCount += currentRow.count('.')

        def newRow = ''
        for (int j = 0; j < currentRow.size(); j++) {
            def left = j > 0 ? currentRow[j - 1] : '.'
            def center = currentRow[j]
            def right = j < currentRow.size() - 1 ? currentRow[j + 1] : '.'

            if ((left == '^' && center == '^' && right == '.') ||
                (center == '^' && right == '^' && left == '.') ||
                (left == '^' && right == '.' && center == '.') ||
                (right == '^' && left == '.' && center == '.')) {
                newRow += '^'
            } else {
                newRow += '.'
            }
        }

        currentRow = newRow
    }

    return safeCount
}

println countSafeTiles(input, 40)
println countSafeTiles(input, 400000)