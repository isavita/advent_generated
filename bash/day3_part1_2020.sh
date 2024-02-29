
#!/bin/bash

forest=($(cat input.txt))
trees=0
x=0
width=${#forest[0]}

for (( y=0; y<${#forest[@]}; y+=1 )); do
    if [ "${forest[$y]:$x%width:1}" == "#" ]; then
        ((trees++))
    fi
    ((x+=3))
done

echo $trees
exit 0
