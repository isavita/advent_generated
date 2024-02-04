
import math

SIZE = 119315717514047

class Deck:
    def __init__(self):
        self.step = 1
        self.direction = 1
        self.top = 0

    def deal_into_new_stack(self):
        self.top = (self.top - self.direction*self.step + SIZE) % SIZE
        self.direction *= -1

    def cut_n(self, n):
        self.top = (self.top + self.direction*self.step*n + SIZE) % SIZE

    def deal_with_increment_n(self, n):
        inv = pow(n, -1, SIZE)
        self.step *= inv
        self.top *= inv

    def pick(self, n):
        current = self.top
        for i in range(n):
            current = ((current + self.direction*self.step) % SIZE + SIZE) % SIZE
        return current

deck = Deck()

file = open("input.txt", "r")
for line in file:
    line = line.strip()
    if line == "deal into new stack":
        deck.top = (deck.top - deck.direction*deck.step + SIZE) % SIZE
        deck.direction *= -1
    elif line.startswith("cut"):
        n = int(line.split(" ")[-1])
        deck.top = (deck.top + deck.direction*deck.step*n + SIZE) % SIZE
    elif line.startswith("deal with increment"):
        n = int(line.split(" ")[-1])
        inv = pow(n, -1, SIZE)
        deck.step = (deck.step * inv) % SIZE
        deck.top = (deck.top * inv) % SIZE

offset = 0
increment = 1

file.seek(0)
for line in file:
    line = line.strip()
    if line == "deal into new stack":
        increment *= -1
        offset += increment
    elif line.startswith("cut"):
        n = int(line.split(" ")[-1])
        offset += n*increment
    elif line.startswith("deal with increment"):
        n = int(line.split(" ")[-1])
        increment *= pow(n, -1, SIZE)

size = SIZE
iter = 101741582076661

final_increment = pow(increment, iter, size)
final_offset = (pow(increment, iter, size) - 1) * pow(increment - 1, -1, size) * offset
answer = (2020 * final_increment + final_offset) % size

print(answer)
