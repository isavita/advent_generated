
def apply_mask(value, mask):
    result = list(format(value, '036b'))
    for i in range(36):
        if mask[i] == '1':
            result[i] = '1'
        elif mask[i] == '0':
            result[i] = '0'
    return int("".join(result), 2)

memory = {}
mask = ""
with open("input.txt") as f:
    for line in f:
        if line.startswith("mask"):
            mask = line.split(" = ")[1].strip()
        else:
            address, value = map(int, line.replace("mem[", "").replace("]", "").split(" = "))
            memory[address] = apply_mask(value, mask)

print(sum(memory.values()))
