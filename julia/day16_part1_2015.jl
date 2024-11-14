
function solve_aunt_sue()
    # MFCSAM output reference values
    reference = Dict(
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

    # Read input file
    input_lines = readlines("input.txt")

    # Process each Aunt Sue's details
    for line in input_lines
        # Parse the line
        sue_match = match(r"Sue (\d+): (.+)", line)
        sue_number = parse(Int, sue_match.captures[1])
        
        # Parse compounds
        compounds = Dict{String, Int}()
        for compound_str in split(sue_match.captures[2], ", ")
            compound, value = split(compound_str, ": ")
            compounds[compound] = parse(Int, value)
        end

        # Check if this Sue matches the reference
        is_match = true
        for (compound, value) in compounds
            if reference[compound] != value
                is_match = false
                break
            end
        end

        # Print the matching Sue's number
        if is_match
            println("Aunt Sue number: ", sue_number)
            return
        end
    end
end

# Run the solution
solve_aunt_sue()
