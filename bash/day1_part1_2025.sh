
    p=50
    z=0
    while read -r line; do
        dir=${line:0:1}
        val=${line:1}
        if [ "$dir" == "R" ]; then
            (( p = (p + val) % 100 ))
        else
            (( p = (p - val) % 100 ))
        fi
        if [ $p -lt 0 ]; then (( p += 100 )); fi
        if [ $p -eq 0 ]; then (( z++ )); fi
    done < input.txt
    echo "The password is: $z"
    