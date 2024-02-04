
numbers = [int(line.strip()) for line in open('input.txt', 'r')]

def find_invalid_number(numbers, preamble):
    for i in range(preamble, len(numbers)):
        found = False
        for j in range(i - preamble, i):
            for k in range(j + 1, i):
                if numbers[j] + numbers[k] == numbers[i]:
                    found = True
                    break
            if found:
                break
        if not found:
            return numbers[i]

preamble = 25
print(find_invalid_number(numbers, preamble))
