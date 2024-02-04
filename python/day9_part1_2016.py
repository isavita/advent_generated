
import re

with open("input.txt", "r") as file:
    input_text = file.readline().strip()

def get_decompressed_length(input):
    marker_regex = re.compile(r'\((\d+)x(\d+)\)')
    length = 0
    i = 0
    while i < len(input):
        marker = marker_regex.search(input, i)
        if marker:
            char_count, repeat_count = map(int, marker.groups())
            length += char_count * repeat_count
            i = marker.end() + char_count
        else:
            length += 1
            i += 1
    return length

decompressed_length = get_decompressed_length(input_text)
print(decompressed_length)
