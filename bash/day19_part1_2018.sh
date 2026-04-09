
#!/bin/bash

r=(0 0 0 0 0 0)
o=(); a=(); b=(); c=()

while read -r f1 f2 f3 f4; do
    if [[ $f1 == "#ip" ]]; then
        ir=$f2
        continue
    fi
    case $f1 in
        addr) o+=(0);; addi) o+=(1);; mulr) o+=(2);; muli) o+=(3);;
        banr) o+=(4);; bani) o+=(5);; borr) o+=(6);; bori) o+=(7);;
        setr) o+=(8);; seti) o+=(9);; gtir) o+=(10);; gtri) o+=(11);;
        gtrr) o+=(12);; eqir) o+=(13);; eqri) o+=(14);; eqrr) o+=(15);;
    esac
    a+=("$f2"); b+=("$f3"); c+=("$f4")
done < input.txt

p=0
n=${#o[@]}

while (( p >= 0 && p < n )); do
    r[ir]=$p
    case ${o[p]} in
        0) (( r[c[p]] = r[a[p]] + r[b[p]] )) ;;
        1) (( r[c[p]] = r[a[p]] + b[p] )) ;;
        2) (( r[c[p]] = r[a[p]] * r[b[p]] )) ;;
        3) (( r[c[p]] = r[a[p]] * b[p] )) ;;
        4) (( r[c[p]] = r[a[p]] & r[b[p]] )) ;;
        5) (( r[c[p]] = r[a[p]] & b[p] )) ;;
        6) (( r[c[p]] = r[a[p]] | r[b[p]] )) ;;
        7) (( r[c[p]] = r[a[p]] | b[p] )) ;;
        8) (( r[c[p]] = r[a[p]] )) ;;
        9) (( r[c[p]] = a[p] )) ;;
        10) (( r[c[p]] = a[p] > r[b[p]] )) ;;
        11) (( r[c[p]] = r[a[p]] > b[p] )) ;;
        12) (( r[c[p]] = r[a[p]] > r[b[p]] )) ;;
        13) (( r[c[p]] = a[p] == r[b[p]] )) ;;
        14) (( r[c[p]] = r[a[p]] == b[p] )) ;;
        15) (( r[c[p]] = r[a[p]] == r[b[p]] )) ;;
    esac
    p=${r[ir]}
    ((p++))
done

echo "${r[0]}"
