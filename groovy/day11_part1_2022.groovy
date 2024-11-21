
class Monkey {
    List<Long> items
    Closure<Long> operation
    int div
    int[] next
    int inspections = 0
}

def parse(String s) {
    def m = new Monkey()
    def lines = s.split('\n')
    m.items = lines[1].split(': ')[1].split(', ').collect { it.toLong() }
    def f = lines[2].split('= ')[1].split()
    m.operation = { old ->
        switch (f[1]) {
            case '+': return f[2] == 'old' ? old + old : old + f[2].toLong()
            case '*': return f[2] == 'old' ? old * old : old * f[2].toLong()
        }
    }
    m.div = lines[3].split(' ')[-1].toInteger()
    m.next = [lines[4].split(' ')[-1].toInteger(), lines[5].split(' ')[-1].toInteger()]
    m
}

def monkeyBusiness(List<Monkey> monkeys, int rounds, boolean worry) {
    def div = monkeys.collect { it.div }.inject(1) { a, b -> a * b }
    (1..rounds).each { _ ->
        monkeys.each { m ->
            while (m.items) {
                m.inspections++
                def item = m.operation(m.items.remove(0))
                item = worry ? item % div : item.intdiv(3)
                monkeys[m.next[item % m.div == 0 ? 0 : 1]].items << item
            }
        }
    }
    monkeys.collect { it.inspections }.sort().reverse()[0..1].inject(1) { a, b -> a * b }
}

def input = new File('input.txt').text
def monkeys = input.split('\n\n').collect { parse(it) }
println monkeyBusiness(monkeys, 20, false)
