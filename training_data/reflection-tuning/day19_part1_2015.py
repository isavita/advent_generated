def parse_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
    
    replacements = {}
    for line in lines:
        if '=>' in line:
            key, value = line.strip().split(' => ')
            if key not in replacements:
                replacements[key] = []
            replacements[key].append(value)
        elif line.strip():
            medicine = line.strip()
    
    return replacements, medicine

def generate_molecules(replacements, medicine):
    molecules = set()
    for i in range(len(medicine)):
        for key, values in replacements.items():
            if medicine[i:].startswith(key):
                for value in values:
                    new_molecule = medicine[:i] + value + medicine[i+len(key):]
                    molecules.add(new_molecule)
    return molecules

def main():
    replacements, medicine = parse_input('input.txt')
    unique_molecules = generate_molecules(replacements, medicine)
    print(len(unique_molecules))

if __name__ == "__main__":
    main()
