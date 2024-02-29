
#!/bin/bash

b=$((57*100 + 100000))
c=$((b + 17000))
h=0

for ((x=b; x<=c; x+=17)); do
    prime=true
    for ((i=2; i*i<=x; i++)); do
        if [ $((x%i)) -eq 0 ]; then
            prime=false
            break
        fi
    done
    if [ $prime = false ]; then
        ((h++))
    fi
done

echo $h
exit 0
