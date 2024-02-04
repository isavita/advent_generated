with open('input.txt', 'r') as file:
    data = file.read().strip()

def look_and_say(input_string):
    result = []
    i = 0
    while i < len(input_string):
        count = 1
        while i + 1 < len(input_string) and input_string[i] == input_string[i + 1]:
            i += 1
            count += 1
        result.append(str(count) + input_string[i])
        i += 1
    return ''.join(result)

for _ in range(40):
    data = look_and_say(data)

print(len(data))