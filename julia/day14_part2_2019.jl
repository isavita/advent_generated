
function parseChemical(s)
    parts = split(s, " ")
    amount = parse(Int, parts[1])
    return (name = parts[2], amount = amount)
end

function calculateOre(chem, amount, reactions, ingredients, surplus)
    if chem == "ORE"
        return amount
    end

    if haskey(surplus, chem) && surplus[chem] >= amount
        surplus[chem] -= amount
        return 0
    end

    amount -= get(surplus, chem, 0)
    surplus[chem] = 0
    reaction = reactions[chem]
    times = div(amount + reaction.amount - 1, reaction.amount)
    ore = 0

    for ingredient in ingredients[chem]
        ore += calculateOre(ingredient.name, ingredient.amount*times, reactions, ingredients, surplus)
    end

    surplus[chem] += times*reaction.amount - amount
    return ore
end

function maxFuel(reactions, ingredients, oreAvailable)
    low, high = 0, oreAvailable
    while low < high
        mid = div(low + high + 1, 2)
        if calculateOre("FUEL", mid, reactions, ingredients, Dict()) > oreAvailable
            high = mid - 1
        else
            low = mid
        end
    end
    return low
end

file = open("input.txt")
reactions = Dict()
ingredients = Dict()

for line in eachline(file)
    parts = split(line, " => ")
    output = parseChemical(parts[2])
    inputs = [parseChemical(input) for input in split(parts[1], ", ")]
    reactions[output.name] = output
    ingredients[output.name] = inputs
end

const oreAvailable = 1000000000000
println(maxFuel(reactions, ingredients, oreAvailable))
