function filter_values(values, criteria)
    for i in 1:length(values[1])
        zeros, ones = 0, 0
        for val in values
            if val[i] == '0'
                zeros += 1
            else
                ones += 1
            end
        end
        keep = criteria(zeros, ones)
        values = filter_by_bit(values, i, keep)
        if length(values) == 1
            break
        end
    end
    return values[1]
end

function filter_by_bit(values, bit_index, keep)
    filtered = []
    for val in values
        if val[bit_index] == keep
            push!(filtered, val)
        end
    end
    return filtered
end

file = open("input.txt", "r")
values = readlines(file)
close(file)

oxygen_generator_rating = filter_values(values, (zeros, ones) -> zeros > ones ? '0' : '1')
oxygen_generator_rating_int = parse(Int64, oxygen_generator_rating, base=2)

co2_scrubber_rating = filter_values(values, (zeros, ones) -> zeros <= ones ? '0' : '1')
co2_scrubber_rating_int = parse(Int64, co2_scrubber_rating, base=2)

println(oxygen_generator_rating_int * co2_scrubber_rating_int)