
using Printf

mutable struct Coord
    x::Int
    y::Int
    z::Int
end

mutable struct Brick
    mini::Coord
    maxi::Coord
    basedOn::Vector{Brick}
    support::Vector{Brick}
end

Brick(mini::Coord, maxi::Coord) = Brick(mini, maxi, Brick[], Brick[])

function parseInput(lines::Vector{String})::Vector{Brick}
    bricks = Brick[]
    for line in lines
        parts = split(line, '~')
        mini_vals = parse.(Int, split(parts[1], ','))
        maxi_vals = parse.(Int, split(parts[2], ','))
        mini = Coord(mini_vals[1], mini_vals[2], mini_vals[3])
        maxi = Coord(maxi_vals[1], maxi_vals[2], maxi_vals[3])
        push!(bricks, Brick(mini, maxi))
    end
    return bricks
end

function settle(bricks::Vector{Brick})
    sort!(bricks, by = b -> b.mini.z)

    for i in eachindex(bricks)
        supportZ = 0
        basedBricks = Brick[]

        for j in i-1:-1:1
            isIntersectingX = max(bricks[i].mini.x, bricks[j].mini.x) <= min(bricks[i].maxi.x, bricks[j].maxi.x)
            isIntersectingY = max(bricks[i].mini.y, bricks[j].mini.y) <= min(bricks[i].maxi.y, bricks[j].maxi.y)
            isIntersecting = isIntersectingX && isIntersectingY

            if isIntersecting
                if bricks[j].maxi.z == supportZ
                    push!(basedBricks, bricks[j])
                elseif bricks[j].maxi.z > supportZ
                    supportZ = bricks[j].maxi.z
                    basedBricks = [bricks[j]]
                end
            end
        end

        empty!(bricks[i].basedOn)
        append!(bricks[i].basedOn, basedBricks)

        for basedBrick in basedBricks
            push!(basedBrick.support, bricks[i])
        end

        deltaZ = bricks[i].maxi.z - bricks[i].mini.z
        bricks[i].mini.z = supportZ + 1
        bricks[i].maxi.z = bricks[i].mini.z + deltaZ
    end
    # Re-sort by final Z position might be useful sometimes, but not strictly needed for the disintegration check
    # sort!(bricks, by = b -> b.mini.z)
end

function solve(lines::Vector{String})::Int
    bricks = parseInput(lines)
    settle(bricks)

    cnt = 0
    for brick in bricks
        isDisintegratable = true
        for supportedBrick in brick.support
            if length(supportedBrick.basedOn) < 2
                isDisintegratable = false
                break
            end
        end
        if isDisintegratable
            cnt += 1
        end
    end
    return cnt
end

function main()
    lines = readlines("input.txt")
    result = solve(lines)
    println(result)
end

main()
