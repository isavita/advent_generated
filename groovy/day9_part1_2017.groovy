
def file = new File("input.txt")
def score = 0
def depth = 0
def inGarbage = false
def cancelNext = false

file.eachLine { line ->
    line.each { ch ->
        if (cancelNext) {
            cancelNext = false
            return
        }

        if (inGarbage) {
            if (ch == '!') {
                cancelNext = true
            } else if (ch == '>') {
                inGarbage = false
            }
        } else {
            switch (ch) {
                case '{':
                    depth++
                    break
                case '}':
                    score += depth
                    depth--
                    break
                case '<':
                    inGarbage = true
                    break
            }
        }
    }
}

println score
