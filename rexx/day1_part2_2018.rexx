
main:
    filename = 'input.txt'
    changes.0 = 0
    i = 0
    do while lines(filename) > 0
        i = i + 1
        changes.i = linein(filename)
    end
    changes.0 = i
    call lineout filename

    currentFrequency = 0
    drop seen.
    seen.0 = 1

    do forever
        do j = 1 to changes.0
            currentFrequency = currentFrequency + changes.j
            if seen.currentFrequency = 1 then do
                say currentFrequency
                exit
            end
            seen.currentFrequency = 1
        end
    end
return
