
function solve()
    input = read("input.txt", String)
    tiles = parse_tiles(input)
    edge_size = Int(sqrt(length(tiles)))
    assembled_tiles = backtrack_assemble(tiles, edge_size)
    product = assembled_tiles[1, 1].id * assembled_tiles[1, edge_size].id * assembled_tiles[edge_size, 1].id * assembled_tiles[edge_size, edge_size].id
    return product
end

struct Tile
    contents::Matrix{Char}
    id::Int
end

function parse_tiles(input::String)
    blocks = split(strip(input), "\n\n")
    tiles = Vector{Tile}(undef, length(blocks))
    for (i, block) in enumerate(blocks)
        lines = split(block, "\n")
        tile_id = parse(Int, match(r"Tile (\d+):", lines[1]).captures[1])
        contents = reduce(hcat, collect.(lines[2:end]))
        tiles[i] = Tile(contents, tile_id)
    end
    return tiles
end

function backtrack_assemble(tiles::Vector{Tile}, edge_size::Int)
    assembled_tiles = Matrix{Union{Tile, Nothing}}(nothing, edge_size, edge_size)
    used_indices = falses(length(tiles))
    
    function recurse(row::Int, col::Int)
        if row > edge_size
            return assembled_tiles
        end
        next_row = col == edge_size ? row + 1 : row
        next_col = col == edge_size ? 1 : col + 1

        if !isnothing(assembled_tiles[row, col])
            return recurse(next_row, next_col)
        end

        for (i, t) in enumerate(tiles)
            if !used_indices[i]
                for opt in all_grid_orientations(t.contents)
                    if row > 1
                        current_top_row = opt[1, :]
                        bottom_of_above = assembled_tiles[row-1, col].contents[end, :]
                        if current_top_row != bottom_of_above
                            continue
                        end
                    end
                    if col > 1
                        current_left_col = opt[:, 1]
                        right_col_of_left = assembled_tiles[row, col-1].contents[:, end]
                        if current_left_col != right_col_of_left
                            continue
                        end
                    end
                    
                    t = Tile(opt, t.id)
                    assembled_tiles[row, col] = t
                    used_indices[i] = true
                    result = recurse(next_row, next_col)
                    if !isnothing(result)
                        return result
                    end
                    assembled_tiles[row, col] = nothing
                    used_indices[i] = false
                end
            end
        end
        return nothing
    end
    return recurse(1, 1)
end

function all_grid_orientations(grid::Matrix{Char})
    orientations = [grid]
    for _ in 1:3
        push!(orientations, rotate_grid(orientations[end]))
    end
    for i in 1:4
        push!(orientations, mirror_grid(orientations[i]))
    end
    return orientations
end

function rotate_grid(grid::Matrix{Char})
    return permutedims(reverse(grid, dims=2))
end

function mirror_grid(grid::Matrix{Char})
    return grid[:, end:-1:1]
end

println(solve())
