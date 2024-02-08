def program = []

new File('input.txt').eachLine { line ->
    program.add(line)
}

def registers = [a: 7, b: 0, c: 0, d: 0]

int pc = 0

while (pc < program.size()) {
    def parts = program[pc].split(' ')
    switch (parts[0]) {
        case 'cpy':
            def value = parts[1] in registers.keySet() ? registers[parts[1]] : parts[1] as int
            registers[parts[2]] = value
            pc++
            break
        case 'inc':
            registers[parts[1]]++
            pc++
            break
        case 'dec':
            registers[parts[1]]--
            pc++
            break
        case 'jnz':
            def check = parts[1] in registers.keySet() ? registers[parts[1]] : parts[1] as int
            def offset = parts[2] in registers.keySet() ? registers[parts[2]] : parts[2] as int
            if (check != 0) {
                pc += offset
            } else {
                pc++
            }
            break
        case 'tgl':
            def offset = parts[1] in registers.keySet() ? registers[parts[1]] : parts[1] as int
            def idx = pc + offset
            if (idx >= 0 && idx < program.size()) {
                def toggle = program[idx].split(' ')
                if (toggle.size() == 2) {
                    toggle[0] = toggle[0] == 'inc' ? 'dec' : 'inc'
                } else {
                    toggle[0] = toggle[0] == 'jnz' ? 'cpy' : 'jnz'
                }
                program[idx] = toggle.join(' ')
            }
            pc++
            break
    }
}

println registers['a']