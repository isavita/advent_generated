
def isRegister(x) {
    return x in ['a', 'b', 'c', 'd']
}

def getValue(x, registers) {
    if (isRegister(x)) {
        return registers[x]
    } else {
        return x.toInteger()
    }
}

def executeProgram(instructions, registers) {
    int i = 0
    while (i < instructions.size()) {

        if (i + 5 < instructions.size()) {
            def pattern = instructions[i..<(i + 6)]
            if (pattern[0].startsWith('cpy') &&
                pattern[1].startsWith('inc') &&
                pattern[2].startsWith('dec') &&
                pattern[3].startsWith('jnz') &&
                pattern[4].startsWith('dec') &&
                pattern[5].startsWith('jnz')) {

                def cpy_x = pattern[0].split()[1]
                def cpy_y = pattern[0].split()[2]
                def inc_a = pattern[1].split()[1]
                def dec_c = pattern[2].split()[1]
                def jnz_c = pattern[3].split()[1]
                def jnz_c_offset = pattern[3].split()[2]
                def dec_d = pattern[4].split()[1]
                def jnz_d = pattern[5].split()[1]
                def jnz_d_offset = pattern[5].split()[2]

                if (inc_a == 'a' && dec_c == cpy_y && jnz_c == cpy_y && jnz_c_offset == '-2' &&
                    dec_d == 'd' && jnz_d == 'd' && jnz_d_offset == '-5') {
                    registers['a'] += getValue(cpy_x, registers) * registers['d']
                    registers[cpy_y] = 0
                    registers['d'] = 0
                    i += 6
                    continue
                }
            }
        }
        def parts = instructions[i].split()
        def cmd = parts[0]

        if (cmd == 'tgl') {
            def x = getValue(parts[1], registers)
            def targetIdx = i + x
            if (targetIdx >= 0 && targetIdx < instructions.size()) {
                def targetParts = instructions[targetIdx].split()
                if (targetParts.size() == 2) {
                    if (targetParts[0] == 'inc') {
                        targetParts[0] = 'dec'
                    } else {
                        targetParts[0] = 'inc'
                    }
                } else if (targetParts.size() == 3) {
                    if (targetParts[0] == 'jnz') {
                        targetParts[0] = 'cpy'
                    } else {
                        targetParts[0] = 'jnz'
                    }
                }
                instructions[targetIdx] = targetParts.join(' ')
            }
            i++
            continue
        }


        if (cmd == 'cpy') {
            def x = parts[1]
            def y = parts[2]
            if (isRegister(y)) {
                registers[y] = getValue(x, registers)
            }
            i++
        } else if (cmd == 'inc') {
            def x = parts[1]
            if (isRegister(x)) {
                registers[x]++
            }
            i++
        } else if (cmd == 'dec') {
            def x = parts[1]
            if (isRegister(x)) {
                registers[x]--
            }
            i++
        } else if (cmd == 'jnz') {
            def x = parts[1]
            def y = parts[2]
            if (getValue(x, registers) != 0) {
                i += getValue(y, registers)
            } else {
                i++
            }
        } else {
            i++
        }
    }
}

def main() {
    def instructions = new File('input.txt').readLines()
    def registers = ['a': 12, 'b': 0, 'c': 0, 'd': 0]
    executeProgram(instructions, registers)
    println(registers['a'])
}

main()
