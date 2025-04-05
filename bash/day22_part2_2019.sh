
#!/bin/bash

# Constants from the Python code
SIZE=119315717514047
ITER=101741582076661
CARD=2020
INPUT_FILE="input.txt"

# bc prelude with necessary math functions (modular arithmetic)
# scale=0 ensures integer arithmetic
# mod(a, m): calculates a % m, handling potential negative results from bc's %
# pow_mod(b, e, m): calculates (b^e) % m using binary exponentiation
# inv(n, m): calculates modular multiplicative inverse of n under modulus m
#            using Fermat's Little Theorem (n^(m-2) % m), requires m to be prime.
bc_prelude="
scale=0

define mod(a, m) {
    auto r
    r = a % m
    if (r < 0) {
        r += m
    }
    return r
}

define pow_mod(b, e, m) {
    auto r, ob, oe
    ob = b; oe = e
    r = 1
    b = mod(b, m)
    while (e > 0) {
        if (mod(e, 2) == 1) {
            r = mod(r * b, m)
        }
        b = mod(b * b, m)
        e = e / 2
    }
    return r
}

define inv(n, m) {
    return pow_mod(n, m - 2, m)
}

size = $SIZE
iter = $ITER
card = $CARD
"

# Function to run bc commands with the prelude
run_bc() {
    echo "$bc_prelude $1" | bc
}

main() {
    local offset increment line n inv_n
    offset=0
    increment=1

    # Process the input file once to find the parameters of the linear transformation
    # f(x) = (increment * x + offset) % SIZE after one shuffle sequence
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" == "deal into new stack" ]]; then
            # increment = mod(-increment, size)
            increment=$(run_bc "print mod(-($increment), size)")
            # offset = mod(offset + increment, size)
            offset=$(run_bc "print mod($offset + $increment, size)")
        elif [[ "$line" == cut* ]]; then
            n="${line##* }"
            # offset = mod(offset + n * increment, size)
            offset=$(run_bc "print mod($offset + ($n) * ($increment), size)")
        elif [[ "$line" == deal*with*increment* ]]; then
            n="${line##* }"
            # inv_n = inv(n, size)
            inv_n=$(run_bc "print inv($n, size)")
            # increment = mod(increment * inv_n, size)
            increment=$(run_bc "print mod(($increment) * $inv_n, size)")
             # Note: The original Python code's second loop only updates increment here.
             # A standard implementation would update offset too:
             # offset = mod(offset * inv_n, size)
             # We follow the provided Python code strictly.
        fi
    done < "$INPUT_FILE"

    # Calculate the effect of 'iter' shuffles using modular exponentiation
    # f^iter(x) = (final_increment * x + final_offset) % size
    # final_increment = pow(increment, iter, size)
    # final_offset = offset * (pow(increment, iter, size) - 1) * pow(increment - 1, -1, size) % size

    local final_increment final_offset geom_sum num den inv_den answer

    final_increment=$(run_bc "print pow_mod($increment, iter, size)")

    # Calculate geometric series sum part: (increment^iter - 1) / (increment - 1) mod size
    num=$(run_bc "print mod($final_increment - 1, size)")
    den=$(run_bc "print mod($increment - 1, size)")
    inv_den=$(run_bc "print inv($den, size)")
    geom_sum=$(run_bc "print mod($num * $inv_den, size)")

    final_offset=$(run_bc "print mod($offset * $geom_sum, size)")

    # Calculate the final position of the card
    # answer = mod(card * final_increment + final_offset, size)
    answer=$(run_bc "print mod(card * $final_increment + $final_offset, size)")

    echo "$answer"
}

# Ensure input file exists
if [[ ! -f "$INPUT_FILE" ]]; then
    echo "Error: Input file '$INPUT_FILE' not found." >&2
    exit 1
fi

# Run the main function
main
