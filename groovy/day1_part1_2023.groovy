
def sum = 0

new File("input.txt").eachLine { line ->
    if(line.trim().isEmpty()) return

    def firstDigit = -1
    def lastDigit = -1

    line.each {
        if(Character.isDigit(it as char)) {
            if(firstDigit == -1) firstDigit = it as int
            lastDigit = it as int
        }
    }

    if(firstDigit != -1 && lastDigit != -1) {
        sum += Integer.parseInt("${firstDigit}${lastDigit}")
    }
}

println(sum)
