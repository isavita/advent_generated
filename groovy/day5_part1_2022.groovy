
class SupplyStacks {

    static void main(String[] args) {
        def lines = new File("input.txt").readLines()
        def dividerIndex = lines.findIndexOf { it.trim().isEmpty() }

        // Parse stacks
        def stackLines = lines[0..<dividerIndex - 1]
        def numStacks = (stackLines.last().length() + 1) / 4  // Calculate based on input width
        def stacks = (1..numStacks).collect { [] as Deque<String> } //Using Deque for efficient addFirst/removeFirst

        stackLines.reverse().each { line ->
            for (int i = 0; i < numStacks; i++) {
                def crate = line.length() > (1 + i * 4) ? line[1 + i * 4] : ' '
                if (crate != ' ') {
                    stacks[i].addFirst(crate.toString()) // Corrected: Add to the *front* of the Deque
                }
            }
        }


        // Parse and execute moves
        def moves = lines[(dividerIndex + 1)..<lines.size()]
        moves.each { move ->
            def parts = move.split(' ')
            def quantity = parts[1].toInteger()
            def from = parts[3].toInteger() - 1 // Adjust to 0-based index
            def to = parts[5].toInteger() - 1   // Adjust to 0-based index

             for (int i = 0; i < quantity; i++) {
                if (!stacks[from].isEmpty()) {
                    def crate = stacks[from].removeFirst()
                    stacks[to].addFirst(crate) //Correct operation to add to top of stack
                }
            }
        }

        // Get top crates and print result
        def result = stacks.collect { it.isEmpty()? " " : it.first() }.join('')
        println result
    }
}
