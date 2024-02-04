
import re
from itertools import product

def apply_mask_v1(value, mask):
    value_bin = format(value, '036b')
    result = [value_bin[i] if mask[i] == 'X' else mask[i] for i in range(36)]
    return int(''.join(result), 2)

def apply_mask_v2(address, mask):
    address_bin = format(address, '036b')
    result = ['1' if mask[i] == '1' else address_bin[i] if mask[i] == '0' else 'X' for i in range(36)]
    floating_indices = [i for i, bit in enumerate(result) if bit == 'X']
    floating_combinations = product(['0', '1'], repeat=len(floating_indices))
    addresses = []
    for combination in floating_combinations:
        temp_address = result.copy()
        for i, c in zip(floating_indices, combination):
            temp_address[i] = c
        addresses.append(int(''.join(temp_address), 2))
    return addresses

def main():
    memory = {}
    mask = ''
    with open('input.txt', 'r') as file:
        for line in file:
            if line.startswith('mask'):
                mask = line.split(' = ')[1].strip()
            else:
                address, value = map(int, re.findall(r'\d+', line))
                addresses = apply_mask_v2(address, mask)
                for addr in addresses:
                    memory[addr] = value

    print(sum(memory.values()))

if __name__ == "__main__":
    main()
