
import sys

def main() -> None:
    try:
        lines = open("input.txt", "r", encoding="utf-8").read().splitlines()
    except FileNotFoundError:
        print("Grand total: 0")
        return

    if not lines:
        print("Grand total: 0")
        return

    max_w = max(map(len, lines))

    # columns that are only whitespace across all lines
    is_sep = [
        all(col >= len(line) or line[col].isspace() for line in lines)
        for col in range(max_w)
    ]

    grand_total = 0
    in_block = False
    block_op = '+'
    block_numbers = []

    def finish_block() -> None:
        nonlocal grand_total, block_numbers, block_op
        if not block_numbers:
            return
        if block_op == '*':
            block_res = 1
            for n in block_numbers:
                block_res *= n
        else:  # '+'
            block_res = 0
            for n in block_numbers:
                block_res += n
        grand_total += block_res
        block_numbers.clear()

    for x in range(max_w):
        if not is_sep[x]:
            if not in_block:
                in_block = True
                block_op = '+'
                block_numbers.clear()

            sb = []
            for line in lines:
                if x < len(line):
                    ch = line[x]
                    if ch.isdigit():
                        sb.append(ch)
                    elif ch in '+*':
                        block_op = ch
            if sb:
                block_numbers.append(int(''.join(sb)))
        elif in_block:
            finish_block()
            in_block = False

    if in_block:
        finish_block()

    print(f"Grand total: {grand_total}")

if __name__ == "__main__":
    main()
