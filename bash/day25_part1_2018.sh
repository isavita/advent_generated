
#!/usr/bin/env bash
# Read points
declare -a X Y Z T
while IFS= read -r line; do
    IFS=',' read -r a b c d <<< "$line"
    X+=("$a") Y+=("$b") Z+=("$c") T+=("$d")
done < input.txt

N=${#X[@]}

# Union‑Find
declare -a P
for ((i=0;i<N;i++)); do P[i]=$i; done

find() {
    local x=$1
    while (( P[x] != x )); do
        # path compression
        P[x]=${P[${P[x]}]}
        x=${P[x]}
    done
    echo "$x"
}

union() {
    local a b
    b=$(find "$2")
    a=$(find "$1")
    (( a != b )) && P[a]=$b
}

# Build constellations
for ((i=0;i<N;i++)); do
    for ((j=i+1;j<N;j++)); do
        dx=$(( X[i] - X[j] )); dx=${dx#-}
        dy=$(( Y[i] - Y[j] )); dy=${dy#-}
        dz=$(( Z[i] - Z[j] )); dz=${dz#-}
        dt=$(( T[i] - T[j] )); dt=${dt#-}
        dist=$(( dx + dy + dz + dt ))
        (( dist <= 3 )) && union "$i" "$j"
    done
done

# Count constellations
count=0
for ((i=0;i<N;i++)); do
    (( P[i] == i )) && (( count++ ))
done

echo "$count"
