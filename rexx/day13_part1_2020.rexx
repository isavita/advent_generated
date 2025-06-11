
/* REXX */
main:
    earliestDeparture = linein('input.txt')
    busIDsString = linein('input.txt')
    call stream 'input.txt', 'c', 'close'

    minWaitTime = earliestDeparture
    earliestBusID = 0

    busIDs = translate(busIDsString, ' ', ',')
    do i = 1 to words(busIDs)
        id = word(busIDs, i)
        if id = 'x' then iterate

        waitTime = id - (earliestDeparture // id)
        if waitTime < minWaitTime then do
            minWaitTime = waitTime
            earliestBusID = id
        end
    end

    say earliestBusID * minWaitTime
return
