
def parse_range(s):
    start, end = map(int, s.split('-'))
    return start, end

def main():
    with open("input.txt", "r") as file:
        count = 0
        for line in file:
            pair = line.strip().split(',')
            left = parse_range(pair[0])
            right = parse_range(pair[1])
            if left[0] <= right[1] and left[1] >= right[0]:
                count += 1
        print(count)

if __name__ == "__main__":
    main()
