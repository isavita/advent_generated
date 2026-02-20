
function findGuestIndex(name,     i) {
    for (i in guests) {
        if (guests[i] == name) {
            return i
        }
    }
    guests[++numGuests] = name
    return numGuests
}

function calculateHappiness(arrangement,     i, left, right, happiness) {
    happiness = 0
    for (i = 1; i <= numGuests; i++) {
        left = (i == 1) ? numGuests : i - 1
        right = (i == numGuests) ? 1 : i + 1
        happiness += happinessMatrix[arrangement[i], arrangement[left]]
        happiness += happinessMatrix[arrangement[i], arrangement[right]]
    }
    return happiness
}

function permute(arrangement, start,     i, temp, happiness) {
    if (start == numGuests) {
        happiness = calculateHappiness(arrangement)
        if (happiness > maxHappiness) {
            maxHappiness = happiness
        }
        return
    }
    for (i = start; i <= numGuests; i++) {
        temp = arrangement[start]
        arrangement[start] = arrangement[i]
        arrangement[i] = temp

        permute(arrangement, start + 1)

        temp = arrangement[start]
        arrangement[start] = arrangement[i]
        arrangement[i] = temp
    }
}

BEGIN {
    maxHappiness = 0
    numGuests = 0
    while ((getline line < "input.txt") > 0) {
        split(line, words, " ")
        from = words[1]
        action = words[3]
        change = (words[3] == "gain") ? words[4] : -words[4]
        to = words[11]
        sub(/\.$/, "", to)
        fromIndex = findGuestIndex(from)
        toIndex = findGuestIndex(to)
        happinessMatrix[fromIndex, toIndex] = change + 0
    }
    findGuestIndex("You") # Add "You" as a neutral guest
    for (i = 1; i <= numGuests; i++) {
        for (j = 1; j <= numGuests; j++) {
            if ((i, j) in happinessMatrix == 0) {
                happinessMatrix[i, j] = 0
            }
        }
        arrangement[i] = i
    }
    permute(arrangement, 1)
    print maxHappiness
    exit
}
