
import random
import re

def read_input(file_path):
    with open(file_path, 'r') as file:
        return file.read().strip()

def parse_input(input_str):
    blocks = input_str.split("\n\n")
    starting_material = split_molecules(blocks[1])
    
    graph = {}
    for line in blocks[0].split("\n"):
        parts = line.split(" => ")
        if parts[0] in graph:
            graph[parts[0]].append(parts[1])
        else:
            graph[parts[0]] = [parts[1]]
    
    return graph, starting_material

def split_molecules(input_str):
    return re.findall(r'[A-Z][a-z]*', input_str)

def solve(input_str):
    reverse_graph, starting_mols = parse_input(input_str)
    
    product_to_reactant = {}
    for react, products in reverse_graph.items():
        for p in products:
            if p in product_to_reactant:
                raise Exception("dup found")
            product_to_reactant[p] = react
    
    all_products = list(product_to_reactant.keys())
    
    mol = ''.join(starting_mols)
    steps = 0
    
    while mol != "e":
        change_made = False
        for prod in all_products:
            if prod in mol:
                change_made = True
                count = mol.count(prod)
                steps += count
                mol = mol.replace(prod, product_to_reactant[prod])
                break
        
        if not change_made:
            random.shuffle(all_products)
            mol = ''.join(starting_mols)
            steps = 0
    
    return steps

if __name__ == "__main__":
    input_str = read_input("input.txt")
    print(solve(input_str))
