
/* Rexx */
main:
    numeric digits 50
    call linein 'input.txt' /* Discard first line */
    line = linein('input.txt')
    call stream 'input.txt', 'c', 'close'

    timestamp = 0
    step = 1
    offset = -1

    do while line \= ''
        offset = offset + 1
        parse var line id ',' line
        if id = 'x' then iterate
        do while (timestamp + offset) // id \= 0
            timestamp = timestamp + step
        end
        step = step * id
    end

    say timestamp
exit
