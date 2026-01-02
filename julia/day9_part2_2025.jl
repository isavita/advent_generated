
using Base.Iterators: product

function main()
    data = readlines("input.txt")
    points = Tuple{Int,Int}[]
    uniqX = Set{Int}(); uniqY = Set{Int}()
    for line in data
        line = strip(line)
        isempty(line) && continue
        xs, ys = split(line, ',')
        x = parse(Int, xs); y = parse(Int, ys)
        push!(points, (x, y))
        push!(uniqX, x); push!(uniqY, y)
    end
    isempty(points) && (println("No points found."); return)

    xs = sort(collect(uniqX))
    ys = sort(collect(uniqY))
    xIdx = Dict(v=>i-1 for (i,v) in enumerate(xs))
    yIdx = Dict(v=>i-1 for (i,v) in enumerate(ys))

    w = 2*length(xs) + 1
    h = 2*length(ys) + 1
    colW = zeros(Float64, w)
    rowH = zeros(Float64, h)

    colW[1] = 1.0
    for i in 0:(length(xs)-1)
        colW[2i+2] = 1.0
        colW[2i+3] = i < length(xs)-1 ? max(0, xs[i+2] - xs[i+1] - 1) : 1.0
    end
    rowH[1] = 1.0
    for i in 0:(length(ys)-1)
        rowH[2i+2] = 1.0
        rowH[2i+3] = i < length(ys)-1 ? max(0, ys[i+2] - ys[i+1] - 1) : 1.0
    end

    grid = zeros(UInt8, h, w)
    togrid(p) = (2*xIdx[p[1]] + 2, 2*yIdx[p[2]] + 2)

    n = length(points)
    for i in 1:n
        (gx1, gy1) = togrid(points[i])
        (gx2, gy2) = togrid(points[mod1(i, n)+1 == n+1 ? 1 : i+1])
        if gx1 == gx2
            s, e = minmax(gy1, gy2)
            for y in s:e
                rowH[y] > 0 && (grid[y, gx1] = 1)
            end
        else
            s, e = minmax(gx1, gx2)
            for x in s:e
                colW[x] > 0 && (grid[gy1, x] = 1)
            end
        end
    end

    qx = Int[1]; qy = Int[1]
    grid[1,1] = 2
    head = 1
    while head <= length(qx)
        x = qx[head]; y = qy[head]; head += 1
        if x > 1 && grid[y, x-1] == 0
            grid[y, x-1] = 2; push!(qx, x-1); push!(qy, y)
        end
        if x < w && grid[y, x+1] == 0
            grid[y, x+1] = 2; push!(qx, x+1); push!(qy, y)
        end
        if y > 1 && grid[y-1, x] == 0
            grid[y-1, x] = 2; push!(qx, x); push!(qy, y-1)
        end
        if y < h && grid[y+1, x] == 0
            grid[y+1, x] = 2; push!(qx, x); push!(qy, y+1)
        end
    end

    pref = zeros(Float64, h, w)
    for y in 1:h
        rs = 0.0
        for x in 1:w
            val = grid[y,x] != 2 ? colW[x]*rowH[y] : 0.0
            rs += val
            pref[y,x] = rs + (y>1 ? pref[y-1,x] : 0.0)
        end
    end

    getSum(lx, ly, rx, ry) = begin
        lx, rx = minmax(lx, rx)
        ly, ry = minmax(ly, ry)
        s = pref[ry,rx]
        lx>1 && (s -= pref[ry,lx-1])
        ly>1 && (s -= pref[ly-1,rx])
        (lx>1 && ly>1) && (s += pref[ly-1,lx-1])
        s
    end

    maxArea = 0
    for i in 1:n
        for j in i:n
            (x1,y1) = points[i]
            (x2,y2) = points[j]
            area = (abs(x1 - x2) + 1)*(abs(y1 - y2) + 1)
            area <= maxArea && continue
            (gx1, gy1) = togrid(points[i])
            (gx2, gy2) = togrid(points[j])
            getSum(gx1, gy1, gx2, gy2) == area && (maxArea = area)
        end
    end

    println("Largest valid area: $maxArea")
end

main()
