
def input = new File("input.txt").readLines()

def criteria = [
    children: 3,
    cats: 7,
    samoyeds: 2,
    pomeranians: 3,
    akitas: 0,
    vizslas: 0,
    goldfish: 5,
    trees: 3,
    cars: 2,
    perfumes: 1
]

def matches = input.findAll { sue ->
    def props = sue.tokenize(', |:')
    def num = props[1] as int
    def valid = true
    (2..props.size() - 2).step(2).each { index ->
        def key = props[index]
        def value = props[index + 1] as int
        if (key in ['cats', 'trees']) {
            valid &= value > criteria[key]
        } else if (key in ['pomeranians', 'goldfish']) {
            valid &= value < criteria[key]
        } else {
            valid &= value == criteria[key]
        }
    }
    valid
}

println matches[0].tokenize(' ')[1]
