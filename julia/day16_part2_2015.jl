# Define the function to parse the input data
function parse_input(filename::String)
    aunts = Dict{Int, Dict{String, Int}}()
    open(filename) do file
        for line in eachline(file)
            parts = split(line, ": ", limit=2)
            sue_number = parse(Int, match(r"\d+", parts[1]).match)
            attributes = split(parts[2], ", ")
            properties = Dict{String, Int}()
            for attr in attributes
                key_value = split(attr, ": ")
                properties[key_value[1]] = parse(Int, key_value[2])
            end
            aunts[sue_number] = properties
        end
    end
    return aunts
end

# Define the MFCSAM output
mfcsam_output = Dict(
    "children" => 3,
    "cats" => 7,
    "samoyeds" => 2,
    "pomeranians" => 3,
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => 5,
    "trees" => 3,
    "cars" => 2,
    "perfumes" => 1
)

# Part one: Find the matching Aunt Sue
function find_matching_sue(aunts)
    for (number, properties) in aunts
        if all([properties[key] == value for (key, value) in properties if key in keys(mfcsam_output)])
            return number
        end
    end
    return -1  # If no match found
end

# Part two: Find the real matching Aunt Sue with adjusted criteria
function find_real_matching_sue(aunts)
    for (number, properties) in aunts
        match = true
        for (key, value) in properties
            if key in ["cats", "trees"]
                match &= (value > mfcsam_output[key])
            elseif key in ["pomeranians", "goldfish"]
                match &= (value < mfcsam_output[key])
            elseif key in keys(mfcsam_output)
                match &= (value == mfcsam_output[key])
            end
        end
        if match
            return number
        end
    end
    return -1  # If no match found
end

# Main execution
function main()
    aunts = parse_input("input.txt")
    part_one_answer = find_matching_sue(aunts)
    part_two_answer = find_real_matching_sue(aunts)
    println("Part one answer: $part_one_answer")
    println("Part two answer: $part_two_answer")
end

main()