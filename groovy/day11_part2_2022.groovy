
class Monkey {
    def items = []
    def operation
    int div
    def next = [0, 0]
    long inspections = 0L
}

def parse(String s) {
    def m = new Monkey()
    def lines = s.split("\n")
    lines[1].split(": ")[1].split(", ").each { m.items << it.toInteger() }
    def f = lines[2].split("= ")[1].split()
    if (f[1] == "+") {
        m.operation = (f[2] == "old") ? { old -> old + old } : { old -> old + f[2].toInteger() }
    } else if (f[1] == "*") {
        m.operation = (f[2] == "old") ? { old -> old * old } : { old -> old * f[2].toInteger() }
    }
    m.div = lines[3].split(" divisible by ")[1].toInteger()
    m.next[0] = lines[4].split(" throw to monkey ")[1].toInteger()
    m.next[1] = lines[5].split(" throw to monkey ")[1].toInteger()
    return m
}

long monkeyBusiness(List<Monkey> monkeys, int rounds, boolean worry) {
    long div = 1
    monkeys.each { div *= it.div }

    (0..<rounds).each { _ ->
        monkeys.each { m ->
            while (m.items.size() > 0) {
                m.inspections++
                def item = m.operation(m.items[0])
                item = worry ? item % div : item.intdiv(3)
                monkeys[item % m.div == 0 ? m.next[0] : m.next[1]].items << item
                m.items.remove(0)
            }
        }
    }

    def inspections = monkeys.collect { it.inspections }
    inspections.sort { a, b -> b <=> a }
    return inspections[0] * inspections[1]
}

def static main(args) {
    def monkeys = []
    def s = new File("input.txt").text
    s.split("\n\n").each { monkeys << parse(it) }
    println monkeyBusiness(monkeys, 10000, true)
}
