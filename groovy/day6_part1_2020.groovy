def input = new File("input.txt").text.split("\n\n")

def sum = 0

input.each { group ->
    def answers = group.split().join()
    sum += answers.toSet().size()
}

println sum