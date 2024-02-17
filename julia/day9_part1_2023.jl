
function parse_input(input)
    histories = []
    for line in input
        numbers = parse_string_to_ints(line)
        push!(histories, numbers)
    end
    return histories
end

function parse_string_to_ints(numbers_line)
    numbers = []
    numbers_parts = split(numbers_line)
    for number_str in numbers_parts
        number = parse(Int, number_str)
        push!(numbers, number)
    end
    return numbers
end

function all_zeros(nums)
    for num in nums
        if num != 0
            return false
        end
    end
    return true
end

function calculate_extrapolation(history)
    extrapolations = []
    for i in 2:length(history)
        extrapolation = history[i] - history[i-1]
        push!(extrapolations, extrapolation)
    end
    return extrapolations
end

function calculate_extrapolations(history)
    extrapolations_series = []
    push!(extrapolations_series, history)

    for i in 2:length(history)
        previous_extrapolations = extrapolations_series[i-1]
        if all_zeros(previous_extrapolations)
            return extrapolations_series
        end

        extrapolations = calculate_extrapolation(previous_extrapolations)
        push!(extrapolations_series, extrapolations)
    end

    return extrapolations_series
end

function solve(input)
    histories = parse_input(input)
    res = 0

    for history in histories
        extrapolations_series = calculate_extrapolations(history)

        future_prediction = 0
        for i in length(extrapolations_series):-1:1
            future_prediction = extrapolations_series[i][end] + future_prediction
        end

        res += future_prediction
    end

    return res
end

function read_file(file_name)
    file = read(file_name, String)
    return split(strip(file), '\n')
end

input = read_file("input.txt")
println(solve(input))
