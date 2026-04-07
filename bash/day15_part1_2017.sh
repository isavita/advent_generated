
#!/bin/bash
awk '{v[NR]=$NF} END {
    a=v[1]; b=v[2]
    for (i=0; i<40000000; i++) {
        a = (a * 16807) % 2147483647
        b = (b * 48271) % 2147483647
        if (a % 65536 == b % 65536) c++
    }
    print c + 0
}' input.txt
