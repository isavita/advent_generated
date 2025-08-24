import os
from datasets import load_dataset

def find_missing_solutions():
    # Load the dataset
    print("Loading dataset...")
    dataset = load_dataset("isavita/advent-of-code")
    print("Dataset loaded.")

    # Get the set of julia solution names from the dataset
    print("Filtering for Julia solutions...")
    julia_solutions_in_dataset = set()
    for item in dataset['train']:
        if item['solution_lang'] == 'julia':
            julia_solutions_in_dataset.add(item['name'])
    print(f"Found {len(julia_solutions_in_dataset)} Julia solutions in the dataset.")

    # Get the set of local julia solution names
    local_julia_files = set()
    julia_dir = 'julia'
    if os.path.exists(julia_dir):
        for filename in os.listdir(julia_dir):
            if filename.endswith('.jl'):
                local_julia_files.add(os.path.splitext(filename)[0])
    print(f"Found {len(local_julia_files)} local Julia files.")

    # Find the missing solutions
    missing_solutions = local_julia_files - julia_solutions_in_dataset

    print("\nMissing solutions:")
    if missing_solutions:
        for solution in sorted(list(missing_solutions)):
            print(solution)
    else:
        print("No missing solutions found.")

if __name__ == "__main__":
    find_missing_solutions()
