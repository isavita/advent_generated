
class Scrambler:
    def __init__(self, pw):
        self.pw = list(pw)

    def __str__(self):
        return ''.join(self.pw)

    def swap_positions(self, x, y):
        self.pw[x], self.pw[y] = self.pw[y], self.pw[x]

    def swap_letters(self, x, y):
        self.swap_positions(self.pw.index(x), self.pw.index(y))

    def rotate(self, steps):
        length = len(self.pw)
        steps = steps % length
        if steps < 0:
            steps += length
        self.pw = self.pw[length - steps:] + self.pw[:length - steps]

    def rotate_letter(self, x):
        index = self.pw.index(x)
        if index >= 4:
            index += 1
        self.rotate(index + 1)

    def derotate_letter(self, x):
        index = self.pw.index(x)
        if index % 2 == 1:
            rot = -(index + 1) // 2
        elif index != 0:
            rot = (6 - index) // 2
        else:
            rot = -1
        self.rotate(rot)

    def reverse(self, x, y):
        self.pw[x:y+1] = self.pw[x:y+1][::-1]

    def move(self, x, y):
        ch = self.pw.pop(x)
        self.pw.insert(y, ch)

    def scramble(self, instructions, direction):
        if direction < 0:
            instructions.reverse()
        for instruction in instructions:
            line = instruction.split()
            if instruction.startswith("swap"):
                x, y = line[2], line[-1]
                if line[1] == "position":
                    xi, yi = int(x), int(y)
                    self.swap_positions(xi, yi)
                else:
                    self.swap_letters(x[0], y[0])
            elif instruction.startswith("rotate"):
                if line[1] == "based":
                    if direction > 0:
                        self.rotate_letter(line[-1][0])
                    else:
                        self.derotate_letter(line[-1][0])
                else:
                    x = int(line[2])
                    if line[1] == "left":
                        x = -x
                    if direction < 0:
                        x = -x
                    self.rotate(x)
            elif instruction.startswith("reverse"):
                x, y = line[2], line[-1]
                xi, yi = int(x), int(y)
                self.reverse(xi, yi)
            elif instruction.startswith("move"):
                x, y = line[2], line[-1]
                xi, yi = int(x), int(y)
                if direction < 0:
                    xi, yi = yi, xi
                self.move(xi, yi)
        return self

    def unscramble(self, instructions):
        return self.scramble(instructions, -1)

def reverse_strings(s):
    return s[::-1]

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        instructions = [line.strip() for line in file]

    hashed = "fbgdceah"
    scrambler = Scrambler(hashed)
    result = scrambler.unscramble(instructions)
    print(result)
