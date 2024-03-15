from collections import defaultdict

def parse_chemical(s):
    amount, name = s.split()
    return {'name': name, 'amount': int(amount)}

def calculate_ore(chem, amount, reactions, ingredients, surplus):
    if chem == 'ORE':
        return amount
    if chem in surplus and surplus[chem] >= amount:
        surplus[chem] -= amount
        return 0
    amount -= surplus.get(chem, 0)
    surplus[chem] = 0
    reaction = reactions[chem]
    times = (amount + reaction['amount'] - 1) // reaction['amount']
    ore = 0
    for ingredient in ingredients[chem]:
        ore += calculate_ore(ingredient['name'], ingredient['amount'] * times, reactions, ingredients, surplus)
    surplus[chem] += times * reaction['amount'] - amount
    return ore

def max_fuel(reactions, ingredients, ore_available):
    surplus = {}
    low, high = 0, ore_available // reactions['FUEL']['amount']
    while low < high:
        mid = (low + high + 1) // 2
        if calculate_ore('FUEL', mid, reactions, ingredients, surplus) > ore_available:
            high = mid - 1
        else:
            low = mid
    return low

with open('input.txt', 'r') as file:
    reactions = {}
    ingredients = defaultdict(list)
    for line in file:
        inputs, output = line.strip().split(' => ')
        output_chem = parse_chemical(output)
        reactions[output_chem['name']] = output_chem
        for input_chem in inputs.split(', '):
            ingredients[output_chem['name']].append(parse_chemical(input_chem))

    ore_available = 1000000000000
    print(max_fuel(reactions, ingredients, ore_available))