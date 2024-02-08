
def input = new File("input.txt").text.split("\\n\\n")

def part1 = input.collect { group ->
    group.findAll { it != '\n' }.toSet().size()
}.sum()

def part2 = input.collect { group ->
    def people = group.split("\\n")
    def commonAnswers = people.inject(people[0].toSet()) { common, person -> common.intersect(person.toSet()) }.size()
    commonAnswers
}.sum()

println part1
println part2
