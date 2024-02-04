with open('input.txt', 'r') as file:
    data = file.read().strip()

def decompressed_length_v1(data):
    length = 0
    i = 0
    while i < len(data):
        if data[i] == '(':
            marker_end = data.index(')', i)
            num_chars, repeat = map(int, data[i+1:marker_end].split('x'))
            length += num_chars * repeat
            i = marker_end + num_chars + 1
        else:
            length += 1
            i += 1
    return length

def decompressed_length_v2(data):
    length = 0
    i = 0
    while i < len(data):
        if data[i] == '(':
            marker_end = data.index(')', i)
            num_chars, repeat = map(int, data[i+1:marker_end].split('x'))
            i = marker_end + 1
            length += decompressed_length_v2(data[i:i+num_chars]) * repeat
            i += num_chars
        else:
            length += 1
            i += 1
    return length

print(decompressed_length_v1(data))
print(decompressed_length_v2(data))