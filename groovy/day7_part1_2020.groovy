
def contains = [:]
def file = new File("input.txt")

file.eachLine { line ->
    def parts = line.split(" bags contain ")
    def container = parts[0]
    if (parts[1] == "no other bags.") {
        return
    }
    def containedBags = parts[1].split(", ")
    containedBags.each { bag ->
        def bagName = bag.tokenize()[1..2].join(" ")
        if (!contains.containsKey(bagName)) {
            contains[bagName] = []
        }
        contains[bagName] << container
    }
}

def countCanContain(target, contains) {
    def seen = [:]
    def dfs
    dfs = { bag ->
        contains[bag]?.each { outer ->
            if (!seen.containsKey(outer)) {
                seen[outer] = true
                dfs(outer)
            }
        }
    }
    dfs(target)
    seen.size()
}

def count = countCanContain("shiny gold", contains)
println count
