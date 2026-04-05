
    #!/bin/bash
    awk '
    BEGIN {
        mod = 2147483647; fA = 16807; fB = 48271; cA = 4; cB = 8; lim = 5000000; m = 65536
        getline < "input.txt"; split($0, pA); vA = pA[length(pA)]
        getline < "input.txt"; split($0, pB); vB = pB[length(pB)]
        t = 0
        for (i = 0; i < lim; i++) {
            do { vA = (vA * fA) % mod } while (vA % cA != 0)
            do { vB = (vB * fB) % mod } while (vB % cB != 0)
            if ((vA % m) == (vB % m)) t++
        }
        print t
    }'
    