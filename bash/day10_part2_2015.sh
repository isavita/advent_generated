
#!/bin/bash

look_and_say() {
    echo "$1" | awk 'BEGIN { FS=""; OFS=""; ORS="" }
    {
        if (NF == 0) { exit }
        count = 1
        for (i = 1; i < NF; i++) {
            if ($i == $(i+1)) {
                count++
            } else {
                print count $i
                count = 1
            }
        }
        print count $NF
    }'
}

main() {
    if [[ ! -f "input.txt" ]]; then
        echo "Error: input.txt not found." >&2
        exit 1
    fi

    sequence=$(< input.txt)
    sequence=${sequence%$'\n'} # Strip potential trailing newline

    for (( i=0; i<40; i++ )); do
        sequence=$(look_and_say "$sequence")
    done
    echo "${#sequence}"

    for (( i=0; i<10; i++ )); do
         sequence=$(look_and_say "$sequence")
    done
    echo "${#sequence}"
}

main "$@"
