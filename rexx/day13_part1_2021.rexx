
/* REXX */
call main
exit

main:
    fileName = 'input.txt'
    points. = 0
    pointList = ''

    do while lines(fileName) > 0
        line = linein(fileName)
        if line = '' then leave
        parse var line x ',' y
        key = x'.'y
        if points.key = 0 then do
            points.key = 1
            pointList = pointList key
        end
    end

    parse value linein(fileName) with . 'along' axis '=' value
    call lineout fileName

    newPoints. = 0
    count = 0
    do i = 1 to words(pointList)
        key = word(pointList, i)
        parse var key x '.' y

        if axis = 'x' then do
            if x > value then x = 2 * value - x
        end
        else do
            if y > value then y = 2 * value - y
        end

        newKey = x'.'y
        if newPoints.newKey = 0 then do
            newPoints.newKey = 1
            count = count + 1
        end
    end

    say count
return
