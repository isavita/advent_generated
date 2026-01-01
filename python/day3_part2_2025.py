import sys

TARGET = 12

def main():
    total = 0
    with open("input.txt") as f:
        for line in f:
            line = line.rstrip("\n")
            end = len(line)
            while end and not line[end - 1].isdigit():
                end -= 1
            if end < TARGET:
                continue
            s = line[:end]
            rem = len(s) - TARGET
            stack = []
            for c in s:
                while rem and stack and stack[-1] < c:
                    stack.pop()
                    rem -= 1
                stack.append(c)
            num = int(''.join(stack[:TARGET]))
            total += num
    print(total)

if __name__ == "__main__":
    main()
