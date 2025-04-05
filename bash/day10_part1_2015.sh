
#!/bin/bash

main() {
    read -r data < input.txt

    for (( i=0; i<40; i++ )); do
        # Use Perl for efficient look-and-say transformation
        # s/(.)\1*/.../ge : Replace globally (g), evaluating (e) replacement
        # (.)\1* : Match a character and any immediate repetitions
        # length($&) : Length of the full match (the count)
        # $1 : The captured character
        data=$(echo "$data" | perl -pe 's/(.)\1*/length($&).$1/ge')
    done

    echo "${#data}"
}

main
