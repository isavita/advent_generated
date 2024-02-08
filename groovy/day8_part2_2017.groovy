def registers = [:]
def highestValue = 0

def lines = new File("input.txt").readLines()

lines.each {
    def parts = it.split(" ")
    def register = parts[0]
    def operation = parts[1]
    def amount = parts[2].toInteger()
    def conditionRegister = parts[4]
    def conditionOperator = parts[5]
    def conditionValue = parts[6].toInteger()

    if (!registers.containsKey(register)) {
        registers[register] = 0
    }
    if (!registers.containsKey(conditionRegister)) {
        registers[conditionRegister] = 0
    }

    def condition = false
    switch (conditionOperator) {
        case ">":
            condition = registers[conditionRegister] > conditionValue
            break
        case "<":
            condition = registers[conditionRegister] < conditionValue
            break
        case ">=":
            condition = registers[conditionRegister] >= conditionValue
            break
        case "<=":
            condition = registers[conditionRegister] <= conditionValue
            break
        case "==":
            condition = registers[conditionRegister] == conditionValue
            break
        case "!=":
            condition = registers[conditionRegister] != conditionValue
            break
    }

    if (condition) {
        if (operation == "inc") {
            registers[register] += amount
        } else {
            registers[register] -= amount
        }
        if (registers[register] > highestValue) {
            highestValue = registers[register]
        }
    }
}

println(highestValue)