
#!/usr/bin/awk -f

# ------------------------------------------------------------
# Helper: remove leading zeros (keep at least one digit)
function trim(s,   i) {
    for (i = 1; i <= length(s); i++)
        if (substr(s,i,1) != "0") break
    s = substr(s,i)
    return (s == "") ? "0" : s
}

# ------------------------------------------------------------
# Multiply a decimal string by 2024 (returns a new string)
function mul2024(num,   i,j,k,carry,prod, digit, res, out, len, m) {
    len = length(num)
    # result array (least‑significant digit first)
    for (i = 0; i < len+4; i++) res[i] = 0

    # multiplier digits (most‑significant first)
    m[0]=2; m[1]=0; m[2]=2; m[3]=4   # 2024

    for (i = 0; i < len; i++) {
        digit = substr(num, len-i, 1) + 0
        carry = 0
        for (j = 0; j < 4; j++) {
            prod = digit * m[3-j] + res[i+j] + carry
            res[i+j] = prod % 10
            carry   = int(prod / 10)
        }
        k = i + 4
        while (carry > 0) {
            prod = res[k] + carry
            res[k] = prod % 10
            carry  = int(prod / 10)
            k++
        }
    }

    # find most‑significant non‑zero digit
    for (k = len+3; k > 0 && res[k] == 0; k--);
    out = ""
    for (i = k; i >= 0; i--)
        out = out "" (r = res[i] + 0)   # concatenate digit
    return (out == "") ? "0" : out
}

# ------------------------------------------------------------
BEGIN {
    # read the whole line from input.txt
    while ((getline line < "input.txt") > 0) {
        # split on whitespace
        n = split(line, tokens, /[[:space:]]+/)
        for (i = 1; i <= n; i++) {
            stone = tokens[i]
            if (stone != "")
                cur[stone] += 1
        }
        break   # only first line is needed
    }
    close("input.txt")

    # --------------------------------------------------------
    # simulation – 75 steps
    for (step = 1; step <= 75; step++) {
        delete nxt

        for (stone in cur) {
            cnt = cur[stone]

            if (stone == "0") {
                nxt["1"] += cnt
                continue
            }

            if (length(stone) % 2 == 0) {               # even length → split
                mid = int(length(stone) / 2)
                left  = substr(stone, 1, mid)
                right = substr(stone, mid+1)

                left  = trim(left)
                right = trim(right)

                nxt[left]  += cnt
                nxt[right] += cnt
            } else {                                    # odd length → *2024
                newstone = mul2024(stone)
                nxt[newstone] += cnt
            }
        }

        # swap maps for next iteration
        delete cur
        for (k in nxt) cur[k] = nxt[k]
    }

    # --------------------------------------------------------
    # total number of stones (bignum)
    total = 0
    for (stone in cur)
        total += cur[stone]

    print total
}
