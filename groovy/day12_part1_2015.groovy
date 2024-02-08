def input = new File("input.txt").text

def sum = 0
def matcher = input =~ /-?\d+/
matcher.each {
    sum += it.toInteger()
}

println sum