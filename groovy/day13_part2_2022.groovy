import groovy.json.JsonSlurper

def input = new File('input.txt').text
def packets = []
input.split("\n\n").each { pair ->
    def (first, second) = pair.split("\n")
    packets << new JsonSlurper().parseText(first)
    packets << new JsonSlurper().parseText(second)
}

def divider1 = new JsonSlurper().parseText('[[2]]')
def divider2 = new JsonSlurper().parseText('[[6]]')
packets << divider1
packets << divider2

packets.sort { a, b -> compare(a, b) }

def divider1Pos = packets.findIndexOf { compare(it, divider1) >= 0 }
def divider2Pos = packets.findIndexOf { compare(it, divider2) >= 0 }

println((divider1Pos + 1) * (divider2Pos + 1))

def compare(a, b) {
    if (a instanceof Number && b instanceof Number) {
        return a <=> b
    } else if (a instanceof Number) {
        return compare([a], b)
    } else if (b instanceof Number) {
        return compare(a, [b])
    } else {
        def aa = a as List
        def bb = b as List
        for (int i = 0; i < aa.size() && i < bb.size(); i++) {
            if (c = compare(aa[i], bb[i])) {
                return c
            }
        }
        return aa.size() <=> bb.size()
    }
}