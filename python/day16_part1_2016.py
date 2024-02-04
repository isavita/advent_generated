
def dragon_curve(a):
    b = a[::-1].replace('0', '2').replace('1', '0').replace('2', '1')
    return a + '0' + b

def checksum(data):
    result = ''
    for i in range(0, len(data), 2):
        result += '1' if data[i] == data[i + 1] else '0'
    if len(result) % 2 == 0:
        return checksum(result)
    return result

with open('input.txt', 'r') as file:
    initial_state = file.read().strip()

data = initial_state
while len(data) < 272:
    data = dragon_curve(data)

data = data[:272]
print(checksum(data))
