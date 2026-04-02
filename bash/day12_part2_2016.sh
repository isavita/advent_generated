
#!/bin/bash

awk '{
    op[NR] = $1; arg1[NR] = $2; arg2[NR] = $3
}
END {
    registers["c"] = 1
    for (i = 1; i <= NR; ) {
        v = arg1[i]
        val = (v ~ /[a-d]/ ? registers[v] : v)
        
        if (op[i] == "cpy") {
            registers[arg2[i]] = val
            i++
        } else if (op[i] == "inc") {
            registers[v]++
            i++
        } else if (op[i] == "dec") {
            registers[v]--
            i++
        } else if (op[i] == "jnz") {
            i += (val != 0 ? arg2[i] : 1)
        } else {
            i++
        }
    }
    print registers["a"] + 0
}' input.txt
