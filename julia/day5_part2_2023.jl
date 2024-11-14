
function main()
    lines = readlines("input.txt")
    
    # Parse seed ranges
    seed_ranges = let
        seeds = parse.(Int, split(lines[1][8:end]))
        [(seeds[i], seeds[i+1]) for i in 1:2:length(seeds)]
    end
    
    # Parse maps
    maps = begin
        current_map = RangeMap[]
        all_maps = Vector{RangeMap}[]
        
        for line in lines[3:end]
            if isempty(line)
                continue
            elseif occursin("map:", line)
                if !isempty(current_map)
                    push!(all_maps, current_map)
                    current_map = RangeMap[]
                end
            else
                nums = parse.(Int, split(line))
                if length(nums) == 3
                    push!(current_map, RangeMap(nums[2], nums[1], nums[3]))
                end
            end
        end
        
        if !isempty(current_map)
            push!(all_maps, current_map)
        end
        
        all_maps
    end
    
    # Reverse conversion function
    function reverse_convert_number(number, ranges)
        for r in Iterators.reverse(ranges)
            if r.dest_start <= number < r.dest_start + r.length
                return r.src_start + (number - r.dest_start)
            end
        end
        return number
    end
    
    # Check if seed is in ranges
    function is_in_seed_ranges(seed, seed_ranges)
        for (start, length) in seed_ranges
            if start <= seed < start + length
                return true
            end
        end
        return false
    end
    
    # Find lowest location
    for location in 0:typemax(Int)
        seed = location
        for map_ranges in Iterators.reverse(maps)
            seed = reverse_convert_number(seed, map_ranges)
        end
        
        if is_in_seed_ranges(seed, seed_ranges)
            println(location)
            break
        end
    end
end

struct RangeMap
    src_start::Int
    dest_start::Int
    length::Int
end

main()
