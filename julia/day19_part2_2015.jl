using Random

function parseInput(input)
    blocks = split(input, "\n\n")
    startingMaterial = splitMolecules(blocks[2])

    graph = Dict{String,Vector{String}}()

    for l in split(blocks[1], "\n")
        parts = split(l, " => ")
        if !haskey(graph, parts[1])
            graph[parts[1]] = String[]
        end
        push!(graph[parts[1]], parts[2])
    end

    return graph, startingMaterial
end

function splitMolecules(input)
    molecules = String[]
    for char in input
        code = Int(char)
        if code >= Int('A') && code <= Int('Z')
            push!(molecules, string(char))
        else
            molecules[end] *= string(char)
        end
    end
    return molecules
end

function solve(input)
    reverseGraph, startingMols = parseInput(input)

    productToReactant = Dict{String,String}()
    for (react, products) in reverseGraph
        for p in products
            if haskey(productToReactant, p)
                error("dup found")
            end
            productToReactant[p] = react
        end
    end

    allProducts = collect(keys(productToReactant))

    start = join(startingMols, "")
    mol = start

    steps = 0
    while mol != "e"
        changeMade = false
        for prod in allProducts
            count = length(findall(prod, mol))
            if count <= 0
                continue
            end
            changeMade = true
            steps += count
            mol = replace(mol, prod => productToReactant[prod])

            break
        end

        if !changeMade
            shuffle!(allProducts)
            mol = start
            steps = 0
        end
    end

    return steps
end

input = read("input.txt", String)
println(solve(input))