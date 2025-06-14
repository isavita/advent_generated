
/* REXX */
call main
exit

main:
    fileName = 'input.txt'
    nodeCount = 0
    node. = ''

    call lineout fileName
    do while lines(fileName) > 0
        line = linein(fileName)
        if left(line, 1) \= '/' then iterate

        nodeCount = nodeCount + 1
        node.nodeCount.used = strip(word(line, 3), , 'T')
        node.nodeCount.avail = strip(word(line, 4), , 'T')
    end

    viablePairs = 0
    do i = 1 to nodeCount
        used_i = node.i.used
        if used_i = 0 then iterate

        do j = 1 to nodeCount
            if i = j then iterate

            if used_i <= node.j.avail then
                viablePairs = viablePairs + 1
        end
    end

    say viablePairs
return
