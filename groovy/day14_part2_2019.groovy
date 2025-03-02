
def parseChemical(s) {
    def parts = s.split()
    return [name: parts[1], amount: parts[0].toInteger()]
}

def calculateOre(String chem, long amount, Map<String, Map> reactions, Map<String, List<Map>> ingredients, Map<String, Long> surplus) {
    if (chem == 'ORE') {
        return amount
    }

    if (surplus.containsKey(chem)) {
        long available = surplus[chem]
        if (available >= amount) {
            surplus[chem] = available - amount
            return 0
        } else {
            amount -= available
            surplus[chem] = 0
        }
    }

    def reaction = reactions[chem]
    long times = (amount + reaction.amount - 1) / reaction.amount
    long ore = 0
    for (ingredient in ingredients[chem]) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus)
    }
    surplus.put(chem, (surplus.get(chem) ?: 0) + times * reaction.amount - amount)
    return ore
}

def maxFuel(Map<String, Map> reactions, Map<String, List<Map>> ingredients, long oreAvailable) {
    def surplus = [:] as HashMap<String, Long>
    long low = 0
    long high = oreAvailable / reactions['FUEL'].amount

    while (low < high) {
        long mid = (low + high + 1) / 2
        if (calculateOre('FUEL', mid, reactions, ingredients, surplus) > oreAvailable) {
            high = mid - 1
        } else {
            low = mid
        }
    }
    return low
}

def main() {
    def reactions = [:] as HashMap<String, Map>
    def ingredients = [:] as HashMap<String, List<Map>>

    new File('input.txt').eachLine { line ->
        def parts = line.trim().split(' => ')
        def inputs = parts[0]
        def output = parts[1]

        def outputChem = parseChemical(output)
        reactions.put(outputChem.name, outputChem)

        def inputChems = inputs.split(', ')
        def ingredientList = []
        for (inputChem in inputChems) {
            ingredientList.add(parseChemical(inputChem))
        }
        ingredients.put(outputChem.name, ingredientList)
    }

    long oreAvailable = 1000000000000
    println maxFuel(reactions, ingredients, oreAvailable)
}

main()
