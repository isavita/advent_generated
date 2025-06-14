
/* REXX */
call main
exit

main:
    procedure
    fileName = 'input.txt'
    vertices.0 = 1
    vertices.1.x = 0
    vertices.1.y = 0
    currentX = 0
    currentY = 0

    do while lines(fileName) > 0
        line = linein(fileName)
        parse var line dir ' ' len .
        dx = 0
        dy = 0
        select
            when dir = 'U' then dy = -1
            when dir = 'D' then dy = 1
            when dir = 'L' then dx = -1
            when dir = 'R' then dx = 1
        end
        currentX = currentX + dx * len
        currentY = currentY + dy * len
        i = vertices.0 + 1
        vertices.i.x = currentX
        vertices.i.y = currentY
        vertices.0 = i
    end
    call lineout fileName

    n = vertices.0
    area = 0
    perimeter = 0
    do i = 1 to n
        p1x = vertices.i.x
        p1y = vertices.i.y
        next_i = i + 1
        if next_i > n then next_i = 1
        p2x = vertices.next_i.x
        p2y = vertices.next_i.y
        area = area + (p1x * p2y - p1y * p2x)
        perimeter = perimeter + abs(p1x - p2x) + abs(p1y - p2y)
    end

    shoelaceArea = abs(area) % 2
    totalArea = shoelaceArea + perimeter % 2 + 1
    say totalArea
    return
