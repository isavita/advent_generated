
    #!/bin/bash
    input=$(tr -d '\n\r' < input.txt)
    maxEnd=0
    ranges=()
    IFS=',' read -ra parts <<< "$input"
    for p in "${parts[@]}"; do
        [[ -z "$p" ]] && continue
        IFS='-' read -r s e <<< "$p"
        ranges+=("$s $e")
        (( e > maxEnd )) && maxEnd=$e
    done

    maxK=$(( (${#maxEnd} + 1) / 2 ))
    declare -A ids
    for (( k=1; k<=maxK; k++ )); do
        pow10k=$(( 10**k ))
        mult=$(( pow10k + 1 ))
        minS=$(( k == 1 ? 1 : 10**(k-1) ))
        maxS=$(( pow10k - 1 ))

        for r in "${ranges[@]}"; do
            read -r s e <<< "$r"
            sMin=$(( (s + mult - 1) / mult ))
            sMax=$(( e / mult ))
            
            low=$(( sMin > minS ? sMin : minS ))
            high=$(( sMax < maxS ? sMax : maxS ))
            
            for (( cur=low; cur<=high; cur++ )); do
                ids[$(( cur * mult ))]=1
            done
        done
    done

    sum=0
    for id in "${!ids[@]}"; do
        sum=$(( sum + id ))
    done
    echo "$sum"
    