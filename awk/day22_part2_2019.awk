
function mod(x, m) {
    x %= m
    return x < 0 ? x + m : x
}

function mul(a, b, m,   res) {
    res = 0
    a = mod(a, m)
    b = mod(b, m)
    while (b > 0) {
        if (b % 2 == 1) res = (res + a) % m
        a = (a + a) % m
        b = int(b / 2)
    }
    return res
}

function pow(a, b, m,   res) {
    res = 1
    a = mod(a, m)
    while (b > 0) {
        if (b % 2 == 1) res = mul(res, a, m)
        a = mul(a, a, m)
        b = int(b / 2)
    }
    return res
}

BEGIN {
    M = 119315717514047
    IT = 101741582076661
    P = 2020
    A = 1
    B = 0

    while ((getline < "input.txt") > 0) {
        if (/stack/) {
            nA = -1; nB = -1
        } else if (/cut/) {
            nA = 1; nB = -$NF
        } else {
            nA = $NF; nB = 0
        }
        nA = mod(nA, M)
        nB = mod(nB, M)
        tA = mul(A, nA, M)
        tB = mod(mul(nA, B, M) + nB, M)
        A = tA; B = tB
    }

    rA = 1; rB = 0
    while (IT > 0) {
        if (IT % 2 == 1) {
            trA = mul(rA, A, M)
            trB = mod(mul(A, rB, M) + B, M)
            rA = trA; rB = trB
        }
        tA = mul(A, A, M)
        tB = mod(mul(A, B, M) + B, M)
        A = tA; B = tB
        IT = int(IT / 2)
    }

    printf "%.0f\n", mul(mod(P - rB, M), pow(rA, M - 2, M), M)
}
