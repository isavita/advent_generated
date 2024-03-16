using DelimitedFiles

# Parse input as strings
directions = readdlm("input.txt", String)

# Functions to get neighboring tiles
function get_neighbors(tile)
    return [(tile[1]+1, tile[2]), (tile[1]+1, tile[2]-1), (tile[1], tile[2]-1), 
            (tile[1]-1, tile[2]), (tile[1]-1, tile[2]+1), (tile[1], tile[2]+1)]
end

# Function to simulate flipping tiles
function flip_tiles(directions, days)
    tiles = Dict{Tuple{Int, Int}, Bool}()
    
    for dir in directions
        tile = (0, 0)
        i = 1
        while i <= length(dir)
            if dir[i] == 'e'
                tile = (tile[1]+1, tile[2])
            elseif dir[i] == 'w'
                tile = (tile[1]-1, tile[2])
            elseif dir[i:i+1] == "se"
                tile = (tile[1]+1, tile[2]-1)
                i += 1
            elseif dir[i:i+1] == "sw"
                tile = (tile[1], tile[2]-1)
                i += 1
            elseif dir[i:i+1] == "nw"
                tile = (tile[1]-1, tile[2]+1)
                i += 1
            elseif dir[i:i+1] == "ne"
                tile = (tile[1], tile[2]+1)
                i += 1
            end
            i += 1
        end
        
        tiles[tile] = !get(tiles, tile, false)
    end
    
    for _ in 1:days
        new_tiles = deepcopy(tiles)
        for (tile, _) in tiles
            for neighbor in get_neighbors(tile)
                if !haskey(tiles, neighbor)
                    tiles[neighbor] = false
                end
            end
        end
        
        for (tile, _) in tiles
            count_black = 0
            for neighbor in get_neighbors(tile)
                if get(tiles, neighbor, false)
                    count_black += 1
                end
            end
            if get(tiles, tile, false) && (count_black == 0 || count_black > 2)
                new_tiles[tile] = false
            elseif !get(tiles, tile, false) && count_black == 2
                new_tiles[tile] = true
            end
        end
        
        tiles = deepcopy(new_tiles)
    end
    
    return sum(values(tiles))
end

# Part One
println(flip_tiles(directions, 0))

# Part Two
println(flip_tiles(directions, 100))