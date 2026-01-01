import sys

def main():
    data = open("input.txt").read().split()
    pos = 50
    cnt = 0
    for token in data:
        if not token:
            continue
        amt = int(token[1:])
        if token[0] == 'R':
            pos += amt
        else:
            pos -= amt
        pos %= 100
        if pos == 0:
            cnt += 1
    print(cnt)

if __name__ == "__main__":
    main()
