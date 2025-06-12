
/* REXX */
call main

main:
    call stream 'input.txt', 'c', 'open read'
    height = 0
    dist. = -1
    do y = 1 while lines('input.txt') > 0
        height = y
        line = linein('input.txt')
        if y = 1 then width = length(line)
        do x = 1 to width
            char = substr(line, x, 1)
            grid.y.x = char
            if char = 'S' then do
                startX = x
                startY = y
            end
        end
    end
    call stream 'input.txt', 'c', 'close'

    queue. = ''
    queue.1 = startY'.'startX
    head = 1
    tail = 1
    dist.startY.startX = 0

    dx.1 = 0; dy.1 = -1
    dx.2 = 0; dy.2 = 1
    dx.3 = 1; dy.3 = 0
    dx.4 = -1; dy.4 = 0

    do while head <= tail
        parse var queue.head currY '.' currX
        head = head + 1
        currDist = dist.currY.currX

        do i = 1 to 4
            newX = currX + dx.i
            newY = currY + dy.i

            if newX > 0 & newX <= width & newY > 0 & newY <= height then do
                if grid.newY.newX \= '#' & dist.newY.newX = -1 then do
                    dist.newY.newX = currDist + 1
                    tail = tail + 1
                    queue.tail = newY'.'newX
                end
            end
        end
    end

    max_steps = 64
    count = 0
    do y = 1 to height
        do x = 1 to width
            d = dist.y.x
            if d >= 0 & d <= max_steps & d // 2 = 0 then do
                count = count + 1
            end
        end
    end

    say count
return
