
/* REXX */
call main

main:
    checksum1 = 0
    checksum2 = 0
    filename = 'input.txt'

    call stream filename, 'c', 'OPEN READ'
    do while lines(filename) > 0
        line = linein(filename)
        if line = '' then iterate

        drop row.
        row.0 = words(line)
        if row.0 = 0 then iterate

        do i = 1 to row.0
            row.i = word(line, i)
        end

        min_val = row.1
        max_val = row.1
        do i = 2 to row.0
            min_val = min(min_val, row.i)
            max_val = max(max_val, row.i)
        end
        checksum1 = checksum1 + (max_val - min_val)

        row_found = 0
        do i = 1 to row.0 while \row_found
            do j = 1 to row.0 while \row_found
                if i = j then iterate
                if row.i // row.j = 0 then do
                    checksum2 = checksum2 + (row.i % row.j)
                    row_found = 1
                end
            end
        end
    end
    call stream filename, 'c', 'CLOSE'

    say checksum1
    say checksum2
return
