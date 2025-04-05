
#!/bin/bash

main() {
    sed -e 's/one/one1one/g' \
        -e 's/two/two2two/g' \
        -e 's/three/three3three/g' \
        -e 's/four/four4four/g' \
        -e 's/five/five5five/g' \
        -e 's/six/six6six/g' \
        -e 's/seven/seven7seven/g' \
        -e 's/eight/eight8eight/g' \
        -e 's/nine/nine9nine/g' \
        input.txt |
    sed 's/[^0-9]//g' |
    awk '
    BEGIN { sum = 0 }
    {
        if (length($0) > 0) {
            first = substr($0, 1, 1)
            last = substr($0, length($0), 1)
            sum += (first * 10 + last)
        }
    }
    END { print sum }
    '
}

main
