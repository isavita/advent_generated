
#!/bin/bash
awk '{op[NR]=$1; arg1[NR]=$2; arg2[NR]=$3}
END {
    pc = 1
    while (pc in op) {
        cmd = op[pc]
        a = arg1[pc]
        b = arg2[pc]
        
        v_a = (a ~ /[a-z]/ ? r[a] : a) + 0
        v_b = (b ~ /[a-z]/ ? r[b] : b) + 0

        if (cmd == "set") r[a] = v_b
        else if (cmd == "sub") r[a] -= v_b
        else if (cmd == "mul") { r[a] *= v_b; count++ }
        else if (cmd == "jnz") {
            if (v_a != 0) {
                pc += v_b
                continue
            }
        }
        pc++
    }
    print count + 0
}' input.txt
