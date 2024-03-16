struct Coord
    x::Int
    y::Int
end

struct Grid
    width::Int
    height::Int
    data::Dict{Coord, Char}
end

function build_grid(input::Vector{String}, empty::Char)
    width = length(input[1])
    height = length(input)
    data = Dict{Coord, Char}()
    for y in 1:height
        for x in 1:width
            if input[y][x] != empty
                data[Coord(x, y)] = input[y][x]
            end
        end
    end
    return Grid(width, height, data)
end

function to_string(grid::Grid, empty::Char)
    result = ""
    for y in 1:grid.height
        for x in 1:grid.width
            coord = Coord(x, y)
            if haskey(grid.data, coord)
                result *= grid.data[coord]
            else
                result *= empty
            end
        end
        result *= "\n"
    end
    return result
end

function get_empty_rows(grid::Grid)
    empty_rows = Int[]
    for y in 1:grid.height
        is_empty = true
        for x in 1:grid.width
            if haskey(grid.data, Coord(x, y))
                is_empty = false
                break
            end
        end
        if is_empty
            push!(empty_rows, y)
        end
    end
    return empty_rows
end

function get_empty_cols(grid::Grid)
    empty_cols = Int[]
    for x in 1:grid.width
        is_empty = true
        for y in 1:grid.height
            if haskey(grid.data, Coord(x, y))
                is_empty = false
                break
            end
        end
        if is_empty
            push!(empty_cols, x)
        end
    end
    return empty_cols
end

function calculate_offsets(empty_indexes::Vector{Int}, bound::Int)
    offsets = zeros(Int, bound)
    for idx in empty_indexes
        for i in idx+1:bound
            offsets[i] += 1
        end
    end
    return offsets
end

function expand_grid(grid::Grid, expansion_factor::Int)
    empty_cols = get_empty_cols(grid)
    empty_rows = get_empty_rows(grid)
    num_lines_to_add = expansion_factor - 1

    new_grid = Grid(
        grid.width + length(empty_cols) * num_lines_to_add,
        grid.height + length(empty_rows) * num_lines_to_add,
        Dict{Coord, Char}()
    )

    dx = calculate_offsets(empty_cols, grid.width)
    dy = calculate_offsets(empty_rows, grid.height)

    for y in 1:grid.height
        for x in 1:grid.width
            coord = Coord(x, y)
            if haskey(grid.data, coord)
                new_coord = Coord(x + dx[x] * num_lines_to_add, y + dy[y] * num_lines_to_add)
                new_grid.data[new_coord] = grid.data[coord]
            end
        end
    end

    return new_grid
end

abs(x::Int) = x < 0 ? -x : x

function calculate_length(grid::Grid, c1::Coord, c2::Coord)
    dx = abs(c2.x - c1.x)
    dy = abs(c2.y - c1.y)
    return dx + dy
end

function solve(input::Vector{String}, expansion_factor::Int)
    grid = build_grid(input, '.')
    expanded_grid = expand_grid(grid, expansion_factor)

    res = 0
    already_seen = Set{Coord}()
    for coord1 in keys(expanded_grid.data)
        for coord2 in already_seen
            res += calculate_length(expanded_grid, coord1, coord2)
        end
        push!(already_seen, coord1)
    end

    return res
end

input = readlines("input.txt")
println(solve(input, 1000000))