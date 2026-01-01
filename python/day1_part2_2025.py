
def main():
    dial_size = 100
    cur = 50
    total = 0

    for raw in open("input.txt"):
        line = raw.strip()
        if not line:
            continue

        dirc = line[0]
        amt = int(line[1:])

        if dirc == 'R':
            total += (cur + amt) // dial_size
            cur = (cur + amt) % dial_size
        elif dirc == 'L':
            total += (cur - 1) // dial_size - (cur - amt - 1) // dial_size
            cur = (cur - amt) % dial_size
        else:
            raise ValueError(f"Unknown direction '{dirc}' in line '{line}'")

    print(f"The password is: {total}")


if __name__ == "__main__":
    main()
