import java.util.regex.Pattern

class BagRule {
    String color
    int count
}

def file = new File('input.txt')
def rules = [:]
def ruleRegex = Pattern.compile(/(\d+) (\w+ \w+) bags?[,.]/)

file.eachLine { line ->
    def parts = line.split(' bags contain ')
    def container = parts[0]
    def contents = parts[1]

    if (contents == 'no other bags.') return

    def matches = ruleRegex.matcher(contents)
    while (matches.find()) {
        def count = Integer.parseInt(matches.group(1))
        def color = matches.group(2)
        if (!rules.containsKey(container)) rules[container] = []
        rules[container] << new BagRule(color: color, count: count)
    }
}

def countBags(color, rules) {
    def count = 1
    if (rules.containsKey(color)) {
        rules[color].each { rule ->
            count += rule.count * countBags(rule.color, rules)
        }
    }
    count
}

def totalBags = countBags('shiny gold', rules) - 1
println totalBags