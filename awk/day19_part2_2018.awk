BEGIN { ARGV[1] = "input.txt"; ARGC = 2 }
/^#ip/ { ipr = $2; next }
{ o[n] = $1; A[n] = $2; B[n] = $3; C[n] = $4; n++ }
END {
    r[0] = 1
    for (k = 0; k < 1000; k++) {
        if (ip < 0 || ip >= n) break
        r[ipr] = ip
        a = A[ip]; b = B[ip]; c = C[ip]; v1 = r[a]; v2 = r[b]; op = o[ip]
        if (op == "addr") r[c] = v1 + v2
        else if (op == "addi") r[c] = v1 + b
        else if (op == "mulr") r[c] = v1 * v2
        else if (op == "muli") r[c] = v1 * b
        else if (op == "banr") r[c] = and(v1, v2)
        else if (op == "bani") r[c] = and(v1, b)
        else if (op == "borr") r[c] = or(v1, v2)
        else if (op == "bori") r[c] = or(v1, b)
        else if (op == "setr") r[c] = v1
        else if (op == "seti") r[c] = a
        else if (op == "gtir") r[c] = (a > v2 ? 1 : 0)
        else if (op == "gtri") r[c] = (v1 > b ? 1 : 0)
        else if (op == "gtrr") r[c] = (v1 > v2 ? 1 : 0)
        else if (op == "eqir") r[c] = (a == v2 ? 1 : 0)
        else if (op == "eqri") r[c] = (v1 == b ? 1 : 0)
        else if (op == "eqrr") r[c] = (v1 == v2 ? 1 : 0)
        ip = r[ipr] + 1
    }
    for (i = 0; i < 6; i++) if (r[i] > m) m = r[i]
    for (i = 1; i * i <= m; i++) {
        if (m % i == 0) {
            s += i
            if (i * i < m) s += m / i
        }
    }
    print s
}