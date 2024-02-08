def input = new File("input.txt").text.toInteger()

def solve = { n ->
    def x = 1
    while (x * 3 <= n) x *= 3
    n < x * 2 ? n - x : n - 2 * x
}

println solve(input)