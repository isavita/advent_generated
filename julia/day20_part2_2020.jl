
using Printf

struct Tile
    id::Int
    contents::Matrix{Char}
end

function parse_tiles_from_input(input_str::String)::Vector{Tile}
    tiles = Vector{Tile}()
    for block in split(strip(input_str), "\n\n")
        lines = split(block, "\n")
        tile_id_match = match(r"Tile (\d+):", lines[1])
        tile_id = parse(Int, tile_id_match.captures[1])
        # Efficiently create the character matrix
        contents_vec = [collect(line) for line in lines[2:end] if !isempty(line)]
        contents_mat = permutedims(hcat(contents_vec...)) # Creates rows x cols matrix
        push!(tiles, Tile(tile_id, contents_mat))
    end
    return tiles
end

get_col(grid::Matrix{Char}, first_col::Bool)::String = join(grid[:, first_col ? 1 : end])
get_row(grid::Matrix{Char}, first_row::Bool)::String = join(grid[first_row ? 1 : end, :])

remove_borders_from_grid(grid::Matrix{Char})::Matrix{Char} = grid[2:end-1, 2:end-1]

rotate_grid(grid::Matrix{Char})::Matrix{Char} = rotr90(grid)
mirror_grid(grid::Matrix{Char})::Matrix{Char} = grid[:, end:-1:1] # Horizontal flip

function all_grid_orientations(grid::Matrix{Char})::Vector{Matrix{Char}}
    orientations = Vector{Matrix{Char}}()
    current = grid
    for _ in 1:4
        push!(orientations, current)
        current = rotate_grid(current)
    end
    current = mirror_grid(grid)
    for _ in 1:4
        push!(orientations, current)
        current = rotate_grid(current)
    end
    return orientations
end

function backtrack_assemble(tiles::Vector{Tile}, assembled_tiles::Matrix{Union{Nothing,Tile}}, used_indices::Set{Int}, edge_size::Int)
    idx = findfirst(isnothing, assembled_tiles)
    if idx === nothing
        return assembled_tiles # Solution found
    end
    row, col = Tuple(idx)

    for i in 1:length(tiles)
        if !(i in used_indices)
            tile = tiles[i]
            for orientation in all_grid_orientations(tile.contents)
                valid = true
                # Check top neighbor
                if row > 1
                    above_tile = assembled_tiles[row-1, col]
                    if get_row(orientation, true) != get_row(above_tile.contents, false)
                        valid = false
                    end
                end
                # Check left neighbor
                if valid && col > 1
                    left_tile = assembled_tiles[row, col-1]
                    if get_col(orientation, true) != get_col(left_tile.contents, false)
                        valid = false
                    end
                end

                if valid
                    assembled_tiles[row, col] = Tile(tile.id, orientation)
                    push!(used_indices, i)

                    result = backtrack_assemble(tiles, assembled_tiles, used_indices, edge_size)
                    if result !== nothing
                        return result # Propagate solution
                    end

                    # Backtrack
                    pop!(used_indices, i)
                    assembled_tiles[row, col] = nothing
                end
            end
        end
    end

    return nothing # No solution found from this state
end

function find_monster_coords(image::Matrix{Char})::Union{Nothing, Set{Tuple{Int, Int}}}
    monster_pattern = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ]
    monster_height = length(monster_pattern)
    monster_length = length(monster_pattern[1])
    img_height, img_width = size(image)

    monster_offsets = Set{Tuple{Int, Int}}()
    for r in 1:monster_height
        for c in 1:monster_length
            if monster_pattern[r][c] == '#'
                push!(monster_offsets, (r, c))
            end
        end
    end

    found_monster_coords = Set{Tuple{Int, Int}}()
    monster_found_in_orientation = false

    for r in 1:(img_height - monster_height + 1)
        for c in 1:(img_width - monster_length + 1)
            is_monster_here = true
            current_match_coords = Set{Tuple{Int, Int}}()
            for (dr, dc) in monster_offsets
                img_r, img_c = r + dr - 1, c + dc - 1
                if image[img_r, img_c] != '#'
                    is_monster_here = false
                    break
                end
                 push!(current_match_coords, (img_r, img_c))
            end

            if is_monster_here
                union!(found_monster_coords, current_match_coords)
                monster_found_in_orientation = true
            end
        end
    end

    return monster_found_in_orientation ? found_monster_coords : nothing
end

function solve()
    input_str = read("input.txt", String)
    tiles = parse_tiles_from_input(input_str)
    num_tiles = length(tiles)
    edge_size = Int(sqrt(num_tiles))

    initial_assembled = Matrix{Union{Nothing,Tile}}(nothing, edge_size, edge_size)
    initial_used = Set{Int}()
    
    assembled_tiles_maybe = backtrack_assemble(tiles, initial_assembled, initial_used, edge_size)
    
    if assembled_tiles_maybe === nothing
        println("Error: Could not assemble tiles.")
        return
    end
    
    assembled_tiles = convert(Matrix{Tile}, assembled_tiles_maybe)


    tile_inner_size = size(remove_borders_from_grid(tiles[1].contents), 1)
    image_size = edge_size * tile_inner_size
    final_image = Matrix{Char}(undef, image_size, image_size)

    for r in 1:edge_size
        for c in 1:edge_size
            tile = assembled_tiles[r, c]
            inner_grid = remove_borders_from_grid(tile.contents)
            r_start = (r - 1) * tile_inner_size + 1
            c_start = (c - 1) * tile_inner_size + 1
            r_end = r_start + tile_inner_size - 1
            c_end = c_start + tile_inner_size - 1
            final_image[r_start:r_end, c_start:c_end] = inner_grid
        end
    end

    monster_coords = nothing
    correct_image_orientation = final_image # Default
    for orientation in all_grid_orientations(final_image)
        coords = find_monster_coords(orientation)
        if coords !== nothing
            monster_coords = coords
            correct_image_orientation = orientation
            break
        end
    end
    
    if monster_coords === nothing
        println("Error: Monster not found in any orientation.")
        return
    end

    total_hashes = count(==('#'), correct_image_orientation)
    rough_waters_count = total_hashes - length(monster_coords)

    println(rough_waters_count)

end

function main()
    solve()
end

main()

