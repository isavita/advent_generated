
function main()
    xs = Int[]
    ys = Int[]

    open("input.txt", "r") do f
        for line in eachline(f)
            isempty(line) && continue
            parts = split(line, ',')
            length(parts) == 2 || continue
            x = tryparse(Int, strip(parts[1]))
            y = tryparse(Int, strip(parts[2]))
            (x === nothing) || (y === nothing) && continue
            push!(xs, x)
            push!(ys, y)
        end
    end

    n = length(xs)
    maxArea = n > 0 ? 1 : 0

    @inbounds for i = 1:n-1
        x1 = xs[i]
        y1 = ys[i]
        for j = i+1:n
            w = abs(x1 - xs[j]) + 1
            h = abs(y1 - ys[j]) + 1
            area = w * h
            area > maxArea && (maxArea = area)
        end
    end

    println(maxArea)
end

main()
