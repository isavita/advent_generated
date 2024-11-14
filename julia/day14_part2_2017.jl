
function reverse_section!(arr, start, length, n)
    for i in 0:div(length-1, 2)
        a = mod(start + i, n)
        b = mod(start + length - 1 - i, n)
        arr[a+1], arr[b+1] = arr[b+1], arr[a+1]
    end
end

function knot_hash(input)
    lengths = vcat(Int[Int(c) for c in input], [17, 31, 73, 47, 23])
    list = collect(0:255)
    position = skip = 0

    for _ in 1:64
        for length in lengths
            reverse_section!(list, position, length, 256)
            position += length + skip
            skip += 1
        end
    end

    dense_hash = [reduce(xor, list[i*16+1:(i+1)*16]) for i in 0:15]
    return bytes2hex(UInt8.(dense_hash))
end

function hex_to_binary(hex_str)
    return join(bitstring(parse(UInt8, hex_str[i:i], base=16))[end-3:end] for i in 1:length(hex_str))
end

function dfs!(grid, x, y)
    if x < 1 || x > 128 || y < 1 || y > 128 || grid[x, y] != 1
        return
    end
    grid[x, y] = 0
    dfs!(grid, x-1, y)
    dfs!(grid, x+1, y)
    dfs!(grid, x, y-1)
    dfs!(grid, x, y+1)
end

function main()
    key_string = strip(read("input.txt", String))
    grid = zeros(Int, 128, 128)
    total_used = 0
    regions = 0

    for i in 1:128
        row_key = "$(key_string)-$(i-1)"
        hash = knot_hash(row_key)
        binary_row = hex_to_binary(hash)
        
        for (j, bit) in enumerate(binary_row)
            if bit == '1'
                grid[i, j] = 1
                total_used += 1
            end
        end
    end

    for i in 1:128, j in 1:128
        if grid[i, j] == 1
            regions += 1
            dfs!(grid, i, j)
        end
    end

    println(regions)
end

main()
