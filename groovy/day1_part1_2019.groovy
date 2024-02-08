
def masses = []
def total = 0.0

new File("input.txt").eachLine { line ->
    def m = line as Integer
    masses.add(m)
}

masses.each { mass ->
    total += (Math.floor(mass / 3) - 2)
}

println(total)
