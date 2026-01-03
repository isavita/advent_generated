
using Printf

const INF = 1_000_000_000

function parse_line(line)
    buttons = Vector{Int}[]
    for m in eachmatch(r"\((.*?)\)", line)
        nums = split(m.captures[1], r"[^0-9]+", keepempty=false)
        push!(buttons, parse.(Int, nums) .+ 1)
    end
    targets = Int[]
    m = match(r"\{(.*?)\}", line)
    if m !== nothing
        nums = split(m.captures[1], r"[^0-9]+", keepempty=false)
        append!(targets, parse.(Int, nums))
    end
    return buttons, targets
end

function solve(buttons, targets)
    nc = length(targets)
    nb = length(buttons)
    A = zeros(Float64, nc, nb + 1)
    for j in 1:nb, i in buttons[j]
        if i <= nc
            A[i, j] = 1.0
        end
    end
    for i in 1:nc
        A[i, nb + 1] = Float64(targets[i])
    end

    pivot_col = fill(-1, nc)
    row = 1
    for col in 1:nb
        row > nc && break
        m_val, m_idx = -1.0, -1
        for r in row:nc
            if abs(A[r, col]) > m_val
                m_val = abs(A[r, col])
                m_idx = r
            end
        end
        m_val < 1e-9 && continue
        if m_idx != row
            for c in col:nb+1
                A[row, c], A[m_idx, c] = A[m_idx, c], A[row, c]
            end
        end
        p_val = A[row, col]
        for c in col:nb+1; A[row, c] /= p_val; end
        for r in 1:nc
            if r != row && abs(A[r, col]) > 1e-9
                factor = A[r, col]
                for c in col:nb+1; A[r, c] -= factor * A[row, c]; end
            end
        end
        pivot_col[row] = col
        row += 1
    end

    rank = row - 1
    for r in (rank+1):nc
        if abs(A[r, nb+1]) > 1e-6; return -1; end
    end

    is_pivot = fill(false, nb)
    for r in 1:rank
        if pivot_col[r] != -1
            is_pivot[pivot_col[r]] = true
        end
    end
    free_vars = findall(.!is_pivot)
    nf = length(free_vars)

    max_presses = fill(0, nb)
    for i in 1:nb
        limit = INF
        has_counter = false
        for c_idx in buttons[i]
            if c_idx <= nc
                limit = min(limit, targets[c_idx])
                has_counter = true
            end
        end
        max_presses[i] = has_counter ? limit : 0
    end

    p = sortperm(max_presses[free_vars])
    free_vars = free_vars[p]

    best_res = Ref(INF)
    free_vals = zeros(Int, nf)
    presses = zeros(Int, nb)

    function compute()
        for i in 1:nf; presses[free_vars[i]] = free_vals[i]; end
        for r in 1:rank
            c = pivot_col[r]
            v = A[r, nb+1]
            for fv in free_vars
                v -= A[r, fv] * presses[fv]
            end
            iv = round(Int, v)
            if abs(v - iv) > 1e-6 || iv < 0 || iv > max_presses[c]; return -1; end
            presses[c] = iv
        end
        return sum(presses)
    end

    function backtrack(idx, cur)
        if cur >= best_res[]; return; end
        if idx > nf
            s = compute()
            if s > 0 && s < best_res[]; best_res[] = s; end
            return
        end
        fv = free_vars[idx]
        for v in 0:max_presses[fv]
            free_vals[idx] = v
            backtrack(idx + 1, cur + v)
        end
    end

    backtrack(1, 0)
    return best_res[] == INF ? -1 : best_res[]
end

function main()
    input_file = "input.txt"
    if !isfile(input_file)
        return
    end
    total = 0
    for line in eachline(input_file)
        line = strip(line)
        if isempty(line)
            continue
        end
        buttons, targets = parse_line(line)
        if isempty(buttons) || isempty(targets)
            continue
        end
        res = solve(buttons, targets)
        if res > 0
            total += res
        end
    end
    println(total)
end

main()
