/* REXX program to process input from "input.txt" */
file = 'input.txt'
address cmd 'drop ' file
address cmd 'say "Processing "' file

input = ''
do while lines(file) > 0
    input = input || linein(file)
end

width = 25
height = 6
layerSize = width * height
minZeros = layerSize + 1
result = 0

do i = 1 to length(input) by layerSize
    layer = substr(input, i, layerSize)
    zeroCount = 0
    oneCount = 0
    twoCount = 0

    do j = 1 to layerSize
        char = substr(layer, j, 1)
        select
            when char = '0' then zeroCount = zeroCount + 1
            when char = '1' then oneCount = oneCount + 1
            when char = '2' then twoCount = twoCount + 1
        end
    end

    if zeroCount < minZeros then do
        minZeros = zeroCount
        result = oneCount * twoCount
    end
end

say result