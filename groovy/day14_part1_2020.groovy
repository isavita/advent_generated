def applyMask(value, mask) {
    def result = 0
    for (int i = 0; i < 36; i++) {
        def bitValue = 1L << (35 - i)
        if (mask[i] == '1') {
            result |= bitValue
        } else if (mask[i] == 'X') {
            result |= (value & bitValue)
        }
    }
    result
}

def file = new File("input.txt")
def mask = ""
def mem = [:]
def reMem = ~/mem\[(\d+)] = (\d+)/

file.eachLine { line ->
    if (line.startsWith("mask = ")) {
        mask = line - "mask = "
    } else {
        def matches = line =~ reMem
        if (matches) {
            def address = matches[0][1] as long
            def value = matches[0][2] as long
            mem[address] = applyMask(value, mask)
        }
    }
}

def sum = mem.values().sum() as long
println sum