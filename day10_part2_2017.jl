function reverse_section!(list, start, len)
    n = length(list)
    for i in 0:(len - 1) ÷ 2
        idx1 = (start + i) % n + 1
        idx2 = (start + len - 1 - i) % n + 1
        list[idx1], list[idx2] = list[idx2], list[idx1]
    end
end

function knot_hash_round!(list, lengths, pos=0, skip=0)
    n = length(list)
    for len in lengths
        reverse_section!(list, pos, len)
        pos = (pos + len + skip) % n
        skip += 1
    end
    return pos, skip
end

function sparse_to_dense_hash(sparse)
    dense = [foldr(xor, sparse[i:i+15]) for i in 1:16:241]
    return dense
end

function to_hex_string(dense)
    return join(string.(dense, base=16, pad=2))
end

function full_knot_hash(input)
    lengths = [Int(c) for c in input]
    append!(lengths, [17, 31, 73, 47, 23])
    list = collect(0:255)
    pos = 0
    skip = 0
    for _ in 1:64
        pos, skip = knot_hash_round!(list, lengths, pos, skip)
    end
    dense = sparse_to_dense_hash(list)
    hex_string = to_hex_string(dense)
    return hex_string
end

function main()
    input = read("input.txt", String)
    input = strip(input)  # Remove any extraneous whitespace
    result = full_knot_hash(input)
    println(result)
end

main()