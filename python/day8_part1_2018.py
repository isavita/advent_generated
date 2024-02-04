
def read_input():
    with open("input.txt", "r") as file:
        data = list(map(int, file.read().split()))
    return data

def read_node(data):
    num_child_nodes = data.pop(0)
    num_metadata_entries = data.pop(0)
    total = 0
    for _ in range(num_child_nodes):
        total += read_node(data)
    for _ in range(num_metadata_entries):
        total += data.pop(0)
    return total

data = read_input()
result = read_node(data)
print(result)
