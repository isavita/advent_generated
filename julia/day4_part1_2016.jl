using Dates

function is_real_room(room::String)
    parts = split(room, "[")
    checksum = strip(parts[2], ']')
    encrypted_name = split(parts[1], "-")[1:end-1]

    letter_counts = Dict{Char,Int}()
    for part in encrypted_name
        for letter in part
            letter_counts[letter] = get(letter_counts, letter, 0) + 1
        end
    end

    sorted_counts = sort(collect(letter_counts), by=x->(-x[2], x[1]))

    for i in 1:length(checksum)
        if checksum[i] != sorted_counts[i][1]
            return false
        end
    end

    return true
end

function get_sector_id(room::String)
    parts = split(room, "-")
    sector_id_part = parts[end]
    sector_id = parse(Int, split(sector_id_part, "[")[1])
    return sector_id
end

global sum_of_sector_ids = 0
for line in eachline("input.txt")
    if is_real_room(line)
        global sum_of_sector_ids += get_sector_id(line)
    end
end

println(sum_of_sector_ids)