
{
    split($0, cups, "")
    for (i = 1; i <= length($0); i++) {
        circle[cups[i]] = cups[i + 1]
    }
    circle[cups[length($0)]] = cups[1]
    current = cups[1]
    for (i = 1; i <= 100; i++) {
        pickup = circle[current]
        circle[current] = circle[circle[circle[pickup]]]
        destination = current - 1
        if (destination < 1) {
            destination = length($0)
        }
        while (destination == pickup || destination == circle[pickup] || destination == circle[circle[pickup]]) {
            destination--
            if (destination < 1) {
                destination = length($0)
            }
        }
        circle[circle[circle[pickup]]] = circle[destination]
        circle[destination] = pickup
        current = circle[current]
    }
    cup = 1
    result = ""
    for (i = 1; i < length($0); i++) {
        result = result circle[cup]
        cup = circle[cup]
    }
    print result
}
