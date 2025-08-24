struct Program
    weight::Int
    holds::Vector{String}
end

function dfs(name::String, programs::Dict{String,Program})
    program = programs[name]
    total_weight = program.weight

    weights = Dict{Int,Int}()
    for child in program.holds
        weight, balanced = dfs(child, programs)
        if !balanced
            return 0, false
        end
        total_weight += weight
        weights[weight] = get(weights, weight, 0) + 1
    end

    # Check for unbalance
    for (w1, c1) in weights
        for (w2, c2) in weights
            if w1 != w2 && c1 < c2
                unbalanced_program = ""
                for child in program.holds
                    if dfs(child, programs)[1] == w1
                        unbalanced_program = child
                        break
                    end
                end
                println(programs[unbalanced_program].weight + (w2 - w1))
                return 0, false
            end
        end
    end
    return total_weight, true
end

# Step 1: Read Input
data = read("input.txt", String)
lines = split(strip(data), "\n")

# Step 2: Create Data Structure
programs = Dict{String,Program}()

for line in lines
    matches = match(r"([a-z]+) \((\d+)\)(?: -> (.+))?", line)
    name = matches[1]
    weight = parse(Int, matches[2])
    holds = matches[3] !== nothing ? split(matches[3], ", ") : String[]
    programs[name] = Program(weight, holds)
end

# Step 4: Find Root (from Part One)
root = "dtacyn" # Replace this with the root found in Part One

# Step 5: Identify Unbalance
dfs(root, programs)