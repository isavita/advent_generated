function read_weights(filename)
    local weights = {}
    for line in io.lines(filename) do
        table.insert(weights, tonumber(line))
    end
    return weights
end

function sum(t)
    local total = 0
    for i, v in ipairs(t) do
        total = total + v
    end
    return total
end

function product(t)
    local prod = 1
    for i, v in ipairs(t) do
        prod = prod * v
    end
    return prod
end

function combinations(weights, target, group)
    local results = {}
    local function recurse(start, selection, remaining, depth)
        if depth > group then return end
        if remaining == 0 and depth == group then
            table.insert(results, {table.unpack(selection)})
            return
        end
        for i = start, #weights do
            if weights[i] <= remaining then
                table.insert(selection, weights[i])
                recurse(i + 1, selection, remaining - weights[i], depth + 1)
                table.remove(selection)
            end
        end
    end
    recurse(1, {}, target, 0)
    return results
end

function find_optimal_combination(weights, groups)
    table.sort(weights, function(a, b) return a > b end) -- Sort descending for better early pruning
    local total_weight = sum(weights)
    if total_weight % groups ~= 0 then
        return nil
    end

    local target_weight = total_weight / groups
    local min_length = #weights
    local min_qe = math.huge
    local best_combination = nil

    for group_size = 1, #weights do
        local combs = combinations(weights, target_weight, group_size)
        for _, comb in ipairs(combs) do
            local qe = product(comb)
            if #comb < min_length or (#comb == min_length and qe < min_qe) then
                min_length = #comb
                min_qe = qe
                best_combination = comb
            end
        end
        if best_combination then break end
    end

    return best_combination, min_qe
end

-- Read weights from file
local weights = read_weights("input.txt")

-- Part 1: Divide into 3 groups
local best_group_3, qe_3 = find_optimal_combination(weights, 3)
if best_group_3 then
    print("Part 1: Quantum Entanglement (3 groups) =", qe_3)
else
    print("No valid combination found for 3 groups.")
end

-- Part 2: Divide into 4 groups
local best_group_4, qe_4 = find_optimal_combination(weights, 4)
if best_group_4 then
    print("Part 2: Quantum Entanglement (4 groups) =", qe_4)
else
    print("No valid combination found for 4 groups.")
end