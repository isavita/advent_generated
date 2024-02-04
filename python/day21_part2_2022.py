
class Monkey:
    def __init__(self, name):
        self.name = name
        self.val = 0
        self.has_val = False
        self.left = None
        self.right = None
        self.op = ""
    
    def solve(self):
        if self.has_val:
            return self.val, True
        
        if self.left is not None and self.right is not None:
            left, l_ok = self.left.solve()
            right, r_ok = self.right.solve()
            
            if l_ok and r_ok:
                if self.op == "+":
                    return left + right, True
                elif self.op == "-":
                    return left - right, True
                elif self.op == "*":
                    return left * right, True
                elif self.op == "/":
                    return left / right, True
                elif self.op == "==":
                    if left == right:
                        return 0, True
                    else:
                        return 1, True
        return 0, False
    
    def expect(self, x):
        if self.name == "humn":
            return x
        
        left, l_ok = self.left.solve()
        right, r_ok = self.right.solve()
        
        if not l_ok:
            if self.op == "+":
                return self.left.expect(x - right)
            elif self.op == "-":
                return self.left.expect(x + right)
            elif self.op == "*":
                return self.left.expect(x / right)
            elif self.op == "/":
                return self.left.expect(x * right)
            elif self.op == "==":
                return self.left.expect(right)
        
        if not r_ok:
            if self.op == "+":
                return self.right.expect(x - left)
            elif self.op == "-":
                return self.right.expect(left - x)
            elif self.op == "*":
                return self.right.expect(x / left)
            elif self.op == "/":
                return self.right.expect(left / x)
            elif self.op == "==":
                return self.right.expect(left)
        
        raise Exception("impossible")

def parse():
    index = {}

    with open("input.txt", "r") as file:
        for line in file:
            ff = line.strip().split(": ")
            goal = ff[0]
            if goal not in index:
                index[goal] = Monkey(goal)
            
            if ff[1].isdigit():
                index[goal].val = int(ff[1])
                index[goal].has_val = True
                continue
            
            r = ff[1].split()
            left, op, right = r[0], r[1], r[2]
            
            if left not in index:
                index[left] = Monkey(left)
            if right not in index:
                index[right] = Monkey(right)
            
            index[goal].left = index[left]
            index[goal].op = op
            index[goal].right = index[right]
    
    return index

index = parse()
index["humn"].has_val = False
index["root"].op = "=="

print(index["root"].expect(0))
