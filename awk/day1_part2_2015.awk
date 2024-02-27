
# read from file input.txt
{ getline < "input.txt" }

# initialize floor and position
BEGIN {
    floor = 0
    position = 0
}

# iterate over each character in the line
{
    for (i = 1; i <= length($0); i++) {
        position = i
        if (substr($0, i, 1) == "(") {
            floor++
        } else if (substr($0, i, 1) == ")") {
            floor--
            if (floor == -1) {
                print "Santa enters the basement at position " position
                exit
            }
        }
    }
}

# print the final floor
END {
    print "Santa ends on floor " floor
}
