def input = new File("input.txt").text.tokenize(',').collect{ it.toInteger() }
def list = (0..255).toList()
def pos = 0
def skip = 0

input.each { length ->
    def sublist = []
    (0..<length).each { i ->
        sublist.add(list[(pos + i) % list.size()])
    }
    sublist = sublist.reverse()

    (0..<length).each { i ->
        list[(pos + i) % list.size()] = sublist[i]
    }

    pos = (pos + length + skip) % list.size()
    skip++
}

println list[0] * list[1]