
#!/bin/bash

initialState=$(head -n 1 input.txt)
diskLength=272

generateData() {
    data=$1
    length=$2
    while [ ${#data} -lt $length ]; do
        b=""
        for (( i=${#data}-1; i>=0; i-- )); do
            if [ ${data:$i:1} -eq 0 ]; then
                b+="1"
            else
                b+="0"
            fi
        done
        data="$data""0""$b"
    done
    echo "${data:0:$length}"
}

calculateChecksum() {
    data=$1
    while [ $(( ${#data} % 2 )) -eq 0 ]; do
        b=""
        for (( i=0; i<${#data}; i+=2 )); do
            if [ ${data:$i:1} -eq ${data:$((i+1)):1} ]; then
                b+="1"
            else
                b+="0"
            fi
        done
        data="$b"
    done
    echo "$data"
}

data=$(generateData "$initialState" $diskLength)
checksum=$(calculateChecksum "$data")
echo "Checksum: $checksum"
exit 0
