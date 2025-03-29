
using Printf

const SUFFIX = UInt8[17, 31, 73, 47, 23]
const NUM_ELEMENTS = 256
const ROUNDS = 64
const BLOCK_SIZE = 16
const NUM_BLOCKS = NUM_ELEMENTS ÷ BLOCK_SIZE

function knot_hash(input_str::String)::String
    lengths = vcat(Vector{UInt8}(input_str), SUFFIX)
    nums = collect(UInt8(0):UInt8(NUM_ELEMENTS - 1))
    pos = 0
    skip = 0

    @inbounds for _ in 1:ROUNDS
        for len_u8 in lengths
            len = Int(len_u8) # Use Int for arithmetic and ranges
             if len > 1 # Skip reversal for length 0 or 1
                 for i in 0:(len ÷ 2 - 1)
                    idx_a = mod(pos + i, NUM_ELEMENTS)
                    idx_b = mod(pos + len - 1 - i, NUM_ELEMENTS)
                    # Julia uses 1-based indexing
                    nums[idx_a + 1], nums[idx_b + 1] = nums[idx_b + 1], nums[idx_a + 1]
                end
            end
            pos = mod(pos + len + skip, NUM_ELEMENTS)
            skip += 1
        end
    end

    dense_hash = Vector{UInt8}(undef, NUM_BLOCKS)
    @inbounds for i in 0:(NUM_BLOCKS - 1)
        start_idx = i * BLOCK_SIZE + 1 # 1-based index
        block_xor = nums[start_idx]
        for j in 1:(BLOCK_SIZE - 1)
            block_xor = xor(block_xor, nums[start_idx + j])
        end
        dense_hash[i + 1] = block_xor # Store in 1-based index
    end

    # Convert each byte to an 8-bit binary string and join
    # bitstring automatically pads UInt8 to 8 bits
    return join(bitstring.(dense_hash))
end

function main()
    key = ""
    try
        key = strip(read("input.txt", String))
    catch e
        println(stderr, "Error reading input.txt: ", e)
        exit(1)
    end

    total_used = 0
    for i in 0:127
        hash_input = "$key-$i"
        binary_hash = knot_hash(hash_input)
        total_used += count(==('1'), binary_hash)
    end

    println(total_used)
end

main()
