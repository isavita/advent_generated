
/* REXX */
call main
exit

main:
    file = 'input.txt'
    instructions = linein(file)
    call linein file /* Skip blank line */

    map. = ''
    do while lines(file) > 0
        line = translate(linein(file), ' ', '=(,)')
        if line = '' then iterate
        node = word(line, 1)
        map.node.L = word(line, 2)
        map.node.R = word(line, 3)
    end
    call lineout file

    current = 'AAA'
    steps = 0
    i = 1
    len = length(instructions)

    do until current = 'ZZZ'
        steps = steps + 1
        direction = substr(instructions, i, 1)
        current = value('map.'current'.'direction)
        i = i + 1
        if i > len then i = 1
    end

    say steps
return
