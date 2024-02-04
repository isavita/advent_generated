
class SnailNumber:
    def __init__(self, value=None, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def is_regular(self):
        return self.left is None and self.right is None

    def add(self, other):
        new_number = SnailNumber(left=self, right=other)
        return new_number.reduce()

    def reduce(self):
        while True:
            exploded, _, _ = self.explode(0)
            if exploded:
                continue
            if not self.split():
                break
        return self

    def explode(self, depth):
        if self.is_regular():
            return False, 0, 0

        if depth == 4:
            left_value = self.left.value
            right_value = self.right.value
            self.left = None
            self.right = None
            self.value = 0
            return True, left_value, right_value

        exploded, left_value, right_value = self.left.explode(depth + 1)
        if exploded:
            if right_value > 0 and self.right is not None:
                self.right.add_left(right_value)
            return True, left_value, 0

        exploded, left_value, right_value = self.right.explode(depth + 1)
        if exploded:
            if left_value > 0 and self.left is not None:
                self.left.add_right(left_value)
            return True, 0, right_value

        return False, 0, 0

    def add_left(self, value):
        if self.is_regular():
            self.value += value
        else:
            self.left.add_left(value)

    def add_right(self, value):
        if self.is_regular():
            self.value += value
        else:
            self.right.add_right(value)

    def split(self):
        if self.is_regular():
            if self.value >= 10:
                self.left = SnailNumber(value=self.value // 2)
                self.right = SnailNumber(value=(self.value + 1) // 2)
                self.value = None
                return True
            return False
        return self.left.split() or self.right.split()

    def magnitude(self):
        if self.is_regular():
            return self.value
        return 3 * self.left.magnitude() + 2 * self.right.magnitude()

    def deep_copy(self):
        if self.is_regular():
            return SnailNumber(value=self.value)
        return SnailNumber(left=self.left.deep_copy(), right=self.right.deep_copy())

def parse_snail_number(input_str):
    input_str = input_str.strip()
    if input_str[0] != '[':
        return SnailNumber(value=int(input_str))

    balance = 0
    split_index = 0
    for i, char in enumerate(input_str[1:-1], 1):
        if char == '[':
            balance += 1
        elif char == ']':
            balance -= 1
        elif char == ',' and balance == 0:
            split_index = i
            break

    left = parse_snail_number(input_str[1:split_index])
    right = parse_snail_number(input_str[split_index+1:-1])
    return SnailNumber(left=left, right=right)

def main():
    with open("input.txt") as file:
        snail_numbers = [parse_snail_number(line.strip()) for line in file]

    if not snail_numbers:
        print("No snailfish numbers found in the file.")
        return

    largest_magnitude = 0
    for i, a in enumerate(snail_numbers):
        for j, b in enumerate(snail_numbers):
            if i == j:
                continue
            sum1 = a.deep_copy().add(b.deep_copy()).magnitude()
            sum2 = b.deep_copy().add(a.deep_copy()).magnitude()
            largest_magnitude = max(largest_magnitude, sum1, sum2)

    print(largest_magnitude)

if __name__ == "__main__":
    main()
