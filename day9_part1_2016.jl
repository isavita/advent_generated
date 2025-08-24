function decompress_length(input::String)
    i = 1
    decompressed_length = 0
    while i <= length(input)
        if input[i] == '('
            close_paren = findnext(')', input, i)
            marker = input[i+1:close_paren-1]
            split_marker = split(marker, 'x')
            num_chars = parse(Int, split_marker[1])
            repeat_times = parse(Int, split_marker[2])
            decompressed_length += num_chars * repeat_times
            i = close_paren + num_chars + 1
        else
            decompressed_length += 1
            i += 1
        end
    end
    return decompressed_length
end

function main()
    # Read the file content
    file_content = read("input.txt", String)
    # Remove all whitespace from the file content
    file_content = replace(file_content, r"\s" => "")
    # Calculate the decompressed length
    result = decompress_length(file_content)
    println("Decompressed length: ", result)
end

main()