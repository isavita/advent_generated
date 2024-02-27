
#!/usr/bin/awk -f

function isValidTriangle(a, b, c) {
    return a + b > c && a + c > b && b + c > a
}

BEGIN {
    validTriangles = 0
    while ((getline < "input.txt") > 0) {
        split($0, sides)
        if (length(sides) != 3) {
            print "Invalid input format"
            next
        }
        a = sides[1] + 0
        b = sides[2] + 0
        c = sides[3] + 0
        if (isValidTriangle(a, b, c)) {
            validTriangles++
        }
    }
    print validTriangles
}
