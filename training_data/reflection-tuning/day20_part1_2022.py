class Node:
    def __init__(self, value):
        self.value = value
        self.next = None
        self.prev = None

def mix(numbers):
    nodes = [Node(num) for num in numbers]
    n = len(nodes)
    
    # Create circular doubly linked list
    for i in range(n):
        nodes[i].next = nodes[(i + 1) % n]
        nodes[i].prev = nodes[(i - 1) % n]
    
    # Mixing process
    for node in nodes:
        if node.value == 0:
            continue
        
        steps = node.value % (n - 1)
        if steps == 0:
            continue
        
        # Remove node from current position
        node.prev.next = node.next
        node.next.prev = node.prev
        
        # Find new position
        current = node
        for _ in range(steps):
            current = current.next
        
        # Insert node at new position
        node.next = current.next
        node.prev = current
        current.next.prev = node
        current.next = node

    return nodes[0]

def find_grove_coordinates(head):
    # Find node with value 0
    current = head
    while current.value != 0:
        current = current.next
    
    coordinates = []
    for _ in range(3):
        for _ in range(1000):
            current = current.next
        coordinates.append(current.value)
    
    return sum(coordinates)

# Read input from file
with open("input.txt", "r") as f:
    numbers = [int(line.strip()) for line in f]

# Mix the numbers
mixed_head = mix(numbers)

# Find and print the sum of grove coordinates
result = find_grove_coordinates(mixed_head)
print(result)
