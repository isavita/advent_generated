
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
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    end

    surplus[chem] += times * reaction.amount - amount
    return ore
end

reactions = Dict()
ingredients = Dict()
surplus = Dict()

for line in eachline("input.txt")
    parts = split(line, " => ")
    output = parseChemical(parts[2])
    inputs = []
    for inp in split(parts[1], ", ")
        push!(inputs, parseChemical(inp))
    end
    reactions[output.name] = output
    ingredients[output.name] = inputs
end

println(calculateOre("FUEL", 1, reactions, ingredients, surplus))
