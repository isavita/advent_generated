
using Printf

function contains_all_chars(larger::String, smaller::String)
    if length(larger) < length(smaller)
        return false
    end
    larger_set = Set(larger)
    for char in smaller
        if !(char in larger_set)
            return false
        end
    end
    return true
end

function solve()
    lines = readlines("input.txt")
    parsed_input = Vector{Vector{String}}()

    for line in lines
        if isempty(strip(line))
            continue
        end
        parts = [join(sort(collect(m.match))) for m in eachmatch(r"[a-g]+", line)]
        if length(parts) != 14
             error("should be 14 parts in each input line: found $(length(parts)) in '$line'")
        end
        push!(parsed_input, parts)
    end

    ans = 0
    index_to_characters = Vector{String}(undef, 10)

    for set_patterns in parsed_input
        signals = set_patterns[1:10]
        outputs = set_patterns[11:14]

        # Identify 1, 4, 7, 8 by unique lengths
        remaining_signals = Vector{String}()
        identified_indices = Set{Int}()

        for (i, mapping) in enumerate(signals)
            len = length(mapping)
            identified = true
            if len == 2
                index_to_characters[2] = mapping # Digit 1
            elseif len == 4
                index_to_characters[5] = mapping # Digit 4
            elseif len == 3
                index_to_characters[8] = mapping # Digit 7
            elseif len == 7
                index_to_characters[9] = mapping # Digit 8
            else
                 identified = false
                 push!(remaining_signals, mapping)
            end
            if identified
                 push!(identified_indices, i)
            end
        end

        # Identify 0, 3, 9 (overlap with 1)
        zero_three_or_nine = Vector{String}()
        next_remaining = Vector{String}()
        for mapping in remaining_signals
             if contains_all_chars(mapping, index_to_characters[2]) # Overlaps with 1
                 push!(zero_three_or_nine, mapping)
             else
                 push!(next_remaining, mapping)
             end
        end
        remaining_signals = next_remaining

        # Find 3 (len 5 among 0,3,9)
        local idx3 = -1
        for (i, maybe039) in enumerate(zero_three_or_nine)
            if length(maybe039) == 5
                index_to_characters[4] = maybe039 # Digit 3
                idx3 = i
                break
            end
        end
        filter!(x -> x != index_to_characters[4], zero_three_or_nine) # Remove 3

        # Find 9 (overlaps with 4 among remaining 0,9)
        local idx9 = -1
        for (i, maybe09) in enumerate(zero_three_or_nine)
             if contains_all_chars(maybe09, index_to_characters[5]) # Overlaps with 4
                 index_to_characters[10] = maybe09 # Digit 9
                 idx9 = i
                 break
             end
        end
        filter!(x -> x != index_to_characters[10], zero_three_or_nine) # Remove 9

        # Remaining must be 0
        index_to_characters[1] = zero_three_or_nine[1] # Digit 0

        # Identify 6 (len 6 among remaining)
        local idx6 = -1
        for (i, mapping) in enumerate(remaining_signals)
             if length(mapping) == 6
                 index_to_characters[7] = mapping # Digit 6
                 idx6 = i
                 break
             end
        end
         filter!(x -> x != index_to_characters[7], remaining_signals) # Remove 6

        # Identify 5 (is contained within 9 among remaining)
        local idx5 = -1
        for (i, mapping) in enumerate(remaining_signals)
            if contains_all_chars(index_to_characters[10], mapping) # 9 contains 5
                index_to_characters[6] = mapping # Digit 5
                idx5 = i
                break
            end
        end
        filter!(x -> x != index_to_characters[6], remaining_signals) # Remove 5

        # Last one must be 2
        index_to_characters[3] = remaining_signals[1] # Digit 2


        # Decode output
        num = 0
        for out in outputs
            # Julia uses 1-based indexing, map found index to digit (0-9)
            digit_index = findfirst(==(out), index_to_characters)
            if digit_index === nothing
                error("Output pattern $out not found in decoded map")
            end
            num = num * 10 + (digit_index - 1)
        end
        ans += num
    end

    println(ans)
end

function main()
    solve()
end

main()
