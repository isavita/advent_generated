
    max=0
    x=()
    y=()
    while IFS=',' read -r xi yi; do
        x+=("$xi")
        y+=("$yi")
    done < input.txt

    n=${#x[@]}
    for ((i=0; i<n; i++)); do
        xi=${x[i]}
        yi=${y[i]}
        for ((j=i; j<n; j++)); do
            xj=${x[j]}
            yj=${y[j]}
            
            # Absolute difference
            dx=$(( xi - xj ))
            [ $dx -lt 0 ] && dx=$(( -dx ))
            dy=$(( yi - yj ))
            [ $dy -lt 0 ] && dy=$(( -dy ))
            
            area=$(( (dx + 1) * (dy + 1) ))
            [ $area -gt $max ] && max=$area
        done
    done
    echo $max
    