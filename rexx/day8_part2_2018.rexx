
/* Rexx */
call main
exit

main:
    parse value linein('input.txt') with line
    count = words(line)
    do i = 1 to count
        nums.i = word(line, i)
    end

    idx = 1
    call sum_metadata
    say result

    idx = 1
    call node_value
    say result
return

sum_metadata:
    procedure expose nums. idx
    num_child = nums.idx; idx = idx + 1
    num_metadata = nums.idx; idx = idx + 1
    sum = 0
    do i = 1 to num_child
        call sum_metadata
        sum = sum + result
    end
    do i = 1 to num_metadata
        sum = sum + nums.idx
        idx = idx + 1
    end
    return sum

node_value:
    procedure expose nums. idx
    num_child = nums.idx; idx = idx + 1
    num_metadata = nums.idx; idx = idx + 1
    value = 0
    if num_child = 0 then do
        do i = 1 to num_metadata
            value = value + nums.idx
            idx = idx + 1
        end
    end
    else do
        drop child_values.
        do i = 1 to num_child
            call node_value
            child_values.i = result
        end
        do i = 1 to num_metadata
            metadata = nums.idx
            idx = idx + 1
            if metadata > 0 & metadata <= num_child then
                value = value + child_values.metadata
        end
    end
    return value
