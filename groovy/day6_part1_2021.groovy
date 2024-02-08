def input = new File("input.txt").text.tokenize(',').collect{it as int}

def lanternfish = input as LinkedList

(1..80).each {
    lanternfish = lanternfish.collect {
        if (it == 0) {
            [6, 8]
        } else {
            it - 1
        }
    }.flatten()
}

println(lanternfish.size())