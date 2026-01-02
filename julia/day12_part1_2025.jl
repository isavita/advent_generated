
using Base.Iterators: enumerate

struct Shape
    pts::Vector{Tuple{Int,Int}}
    n::Int
end

function normalize(s::Shape)
    isempty(s.pts) && return s
    minr = minimum(p->p[1], s.pts)
    minc = minimum(p->p[2], s.pts)
    arr = [(p[1]-minr, p[2]-minc) for p in s.pts]
    sort!(arr, by = x -> (x[1], x[2]))
    Shape(arr, length(arr))
end

rotate(s::Shape) = Shape([(p[2], -p[1]) for p in s.pts], s.n)
flip(s::Shape)   = Shape([(p[1], -p[2]) for p in s.pts], s.n)

function equal(a::Shape, b::Shape)
    a.n != b.n && return false
    for i = 1:a.n
        a.pts[i] != b.pts[i] && return false
    end
    true
end

function generateVariations(base::Shape)
    uniq = Shape[]
    cur = base
    for _ = 1:4
        n = normalize(cur)
        any(v->equal(v,n), uniq) || push!(uniq, n)
        f = flip(cur)
        nf = normalize(f)
        any(v->equal(v,nf), uniq) || push!(uniq, nf)
        cur = rotate(cur)
    end
    uniq
end

function canPlace(rows, cols, grid, s::Shape, r, c)
    @inbounds for p in s.pts
        nr = r + p[1]; nc = c + p[2]
        nr < 0 || nr ≥ rows && return false
        nc < 0 || nc ≥ cols && return false
        grid[nr*cols+nc+1] && return false
    end
    true
end

function place!(grid, s::Shape, r, c, v::Bool)
    @inbounds for p in s.pts
        grid[(r+p[1])*cols+ (c+p[2]) + 1] = v
    end
end

function checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes)
    minReal = typemax(Int); hasReal = false
    for i = 1:arrSize
        i == slackIdx && continue
        counts[i] > 0 && (minReal = min(minReal, shapes[i].n); hasReal = true)
    end
    !hasReal && return true
    avail = counts[slackIdx]
    vis = falses(rows*cols)
    q = Vector{Int}(undef, rows*cols)
    for idx = 0:rows*cols-1
        if !grid[idx+1] && !vis[idx+1]
            qs = 1; qe = 1; q[1] = idx; vis[idx+1] = true
            sz = 0
            while qs ≤ qe
                cur = q[qs]; qs += 1; sz += 1
                r = cur ÷ cols; c = cur % cols
                if r > 0
                    n = cur - cols
                    if !grid[n+1] && !vis[n+1]; vis[n+1]=true; q[qe+=1]=n end
                end
                if r < rows-1
                    n = cur + cols
                    if !grid[n+1] && !vis[n+1]; vis[n+1]=true; q[qe+=1]=n end
                end
                if c > 0
                    n = cur - 1
                    if !grid[n+1] && !vis[n+1]; vis[n+1]=true; q[qe+=1]=n end
                end
                if c < cols-1
                    n = cur + 1
                    if !grid[n+1] && !vis[n+1]; vis[n+1]=true; q[qe+=1]=n end
                end
            end
            if sz < minReal
                if avail ≥ sz
                    avail -= sz
                else
                    return false
                end
            end
        end
    end
    true
end

function solveRec(rows, cols, grid, counts, arrSize, ids, varCounts, variations, slackIdx, shapes)
    empty = findfirst(!, grid)
    empty === nothing && return true
    r = (empty-1) ÷ cols
    c = (empty-1) % cols
    !checkIslands(rows, cols, grid, counts, arrSize, slackIdx, shapes) && return false
    for id in ids
        counts[id] == 0 && continue
        counts[id] -= 1
        vars = variations[id]
        for p in vars
            if canPlace(rows, cols, grid, p, r, c)
                for pt in p.pts
                    grid[(r+pt[1])*cols + (c+pt[2]) + 1] = true
                end
                if solveRec(rows, cols, grid, counts, arrSize, ids, varCounts, variations, slackIdx, shapes)
                    return true
                end
                for pt in p.pts
                    grid[(r+pt[1])*cols + (c+pt[2]) + 1] = false
                end
            end
        end
        counts[id] += 1
    end
    false
end

function main()
    lines = readlines("input.txt")
    maxId = -1
    for l in lines
        s = strip(l)
        isempty(s) && continue
        endswith(s, ":") && (id = parse(Int, s[1:end-1]); maxId = max(maxId, id))
    end
    arrSize = maxId + 2
    slackIdx = maxId + 1
    shapes = [Shape(Tuple{Int,Int}[], 0) for _=1:arrSize]
    parsingShapes = true; curId = -1; curShape = String[]
    regionLines = String[]
    for raw in lines
        s = strip(raw)
        isempty(s) && continue
        if occursin('x', s) && occursin(':', s)
            parsingShapes = false
        end
        if parsingShapes
            if endswith(s, ":")
                if curId != -1 && !isempty(curShape)
                    pts = Tuple{Int,Int}[]
                    for (rr,row) in enumerate(curShape)
                        for cc = 1:length(row)
                            row[cc] == '#' && push!(pts, (rr-1, cc-1))
                        end
                    end
                    shapes[curId+1] = normalize(Shape(pts, length(pts)))
                end
                curId = parse(Int, s[1:end-1])
                curShape = String[]
            else
                push!(curShape, s)
            end
        else
            push!(regionLines, s)
        end
    end
    if curId != -1 && !isempty(curShape)
        pts = Tuple{Int,Int}[]
        for (rr,row) in enumerate(curShape)
            for cc = 1:length(row)
                row[cc] == '#' && push!(pts, (rr-1, cc-1))
            end
        end
        shapes[curId+1] = normalize(Shape(pts, length(pts)))
    end
    shapes[slackIdx+1] = Shape([(0,0)], 1)

    variations = [Shape[] for _=1:arrSize]
    varCounts = zeros(Int, arrSize)
    for i = 1:arrSize
        s = shapes[i]
        s.n > 0 && (vars = generateVariations(s); variations[i]=vars; varCounts[i]=length(vars))
    end

    solved = 0
    for line in regionLines
        parts = split(line, ':')
        length(parts) != 2 && continue
        dims = strip(parts[1]); cnts = strip(parts[2])
        wx, h = parse.(Int, split(dims, 'x'))
        gridSize = wx*h
        pieceCounts = zeros(Int, arrSize)
        totalArea = 0
        toks = split(cnts)
        for i = 1:min(length(toks), arrSize-1)
            c = parse(Int, toks[i])
            c > 0 && (pieceCounts[i] = c; totalArea += c*shapes[i].n)
        end
        totalArea > gridSize && continue
        slack = gridSize - totalArea
        slack > 0 && (pieceCounts[slackIdx+1] = slack)
        ids = Int[]
        for i = 1:arrSize
            pieceCounts[i] > 0 && push!(ids, i)
        end
        sort!(ids, by = i->-shapes[i].n)
        grid = falses(gridSize)
        if solveRec(h, wx, grid, pieceCounts, arrSize, ids, varCounts, variations, slackIdx+1, shapes)
            solved += 1
        end
    end
    println("Number of regions that fit all presents: $solved")
end

main()
