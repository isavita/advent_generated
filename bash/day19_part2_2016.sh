
#!/bin/bash

josephus() {
    local n=$1
    local i=1
    while (( i * 3 <= n )); do
        (( i *= 3 ))
    done
    echo $(( n - i + ((n - 2*i > 0 ? n - 2*i : 0)) ))
}

main() {
    local num_elves
    num_elves=$(<input.txt)
    josephus "$num_elves"
}

main
