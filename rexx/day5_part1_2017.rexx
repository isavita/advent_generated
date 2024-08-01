/* REXX program to solve the maze jump problem */

parse arg inputFile
if inputFile = '' then inputFile = 'input.txt'

ii = 1
file = linein(inputFile)
if file = '' then exit

/* Read input into an array */
do while file \= ''
    offsets.ii = file
    ii = ii + 1
    file = linein(inputFile)
end

steps = 0
position = 1

do while position > 0 & position < ii
    jump = offsets.position
    offsets.position = jump + 1
    position = position + jump
    steps = steps + 1
end

say steps
exit