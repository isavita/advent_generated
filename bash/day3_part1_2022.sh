
#!/bin/bash

main() {
  awk '
    function get_priority(char) {
        val = index("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", char)
        return val
    }

    {
        len = length($0)
        mid = int(len / 2)
        first_half = substr($0, 1, mid)
        second_half = substr($0, mid + 1)

        delete seen
        delete counted_this_line

        for (i = 1; i <= mid; ++i) {
            char = substr(first_half, i, 1)
            seen[char] = 1
        }

        len_second = length(second_half)
        for (i = 1; i <= len_second; ++i) {
            char = substr(second_half, i, 1)
            if (char in seen && !(char in counted_this_line)) {
                total += get_priority(char)
                counted_this_line[char] = 1
            }
        }
    }

    END { print total }
  ' input.txt
}

main
