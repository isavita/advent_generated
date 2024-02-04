with open('input.txt', 'r') as file:
    data = file.read().strip()

data = list(map(int, data.split(',')))

def get_value(data, mode, param):
    if mode == 0:
        return data[param]
    elif mode == 1:
        return param

def run_program(data, input_val):
    i = 0
    diagnostic_code = None

    while i < len(data):
        opcode = data[i] % 100
        mode1 = data[i] // 100 % 10
        mode2 = data[i] // 1000 % 10
        mode3 = data[i] // 10000 % 10

        if opcode == 1:
            val1 = get_value(data, mode1, data[i+1])
            val2 = get_value(data, mode2, data[i+2])
            data[data[i+3]] = val1 + val2
            i += 4
        elif opcode == 2:
            val1 = get_value(data, mode1, data[i+1])
            val2 = get_value(data, mode2, data[i+2])
            data[data[i+3]] = val1 * val2
            i += 4
        elif opcode == 3:
            data[data[i+1]] = input_val
            i += 2
        elif opcode == 4:
            diagnostic_code = get_value(data, mode1, data[i+1])
            i += 2
        elif opcode == 99:
            break
        else:
            i += 1

    return diagnostic_code

result = run_program(data, 1)
print(result)