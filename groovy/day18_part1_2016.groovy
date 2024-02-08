def input = new File("input.txt").text.trim()

def countSafeTiles(input) {
    def count = 0
    for (int i = 0; i < 40; i++) {
        count += input.count('.')

        def newLine = ""
        for (int j = 0; j < input.size(); j++) {
            def left = (j == 0) ? '.' : input[j - 1]
            def center = input[j]
            def right = (j == input.size() - 1) ? '.' : input[j + 1]

            if ((left == '^' && center == '^' && right == '.') || 
                (left == '.' && center == '^' && right == '^') || 
                (left == '^' && center == '.' && right == '.') || 
                (left == '.' && center == '.' && right == '^')) {
                newLine += '^'
            } else {
                newLine += '.'
            }
        }

        input = newLine
    }

    return count
}

println countSafeTiles(input)