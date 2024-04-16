function read_and_process_claims(filename)
    fabric = zeros(Int, 1000, 1000)  # Create a 1000x1000 fabric grid initialized with zeros
    claims = Dict()

    # Open the file and read line by line
    open(filename, "r") do file
        for line in eachline(file)
            # Parse the input line
            id, _, offset, size = split(line, ' ')
            id = parse(Int, id[2:end])  # Remove '#' and convert to integer
            left, top = split(offset[1:end-1], ',') |> x -> parse.(Int, x)  # Remove ':' and split by ','
            width, height = split(size, 'x') |> x -> parse.(Int, x)

            # Update the fabric grid and store each claim
            claims[id] = (left+1, top+1, width, height)
            for i in left+1:left+width
                for j in top+1:top+height
                    fabric[j, i] += 1
                end
            end
        end
    end

    # Calculate the number of square inches of fabric within two or more claims
    overlap_count = count(x -> x >= 2, fabric)

    # Find the claim that does not overlap with any other claim
    intact_claim_id = nothing
    for (id, (left, top, width, height)) in claims
        if all(fabric[j, i] == 1 for i in left:left+width-1, j in top:top+height-1)
            intact_claim_id = id
            break
        end
    end

    return overlap_count, intact_claim_id
end

# Main execution
function main()
    overlap_count, intact_claim_id = read_and_process_claims("input.txt")
    println("Number of square inches within two or more claims: $overlap_count")
    println("ID of the only claim that doesn't overlap: $intact_claim_id")
end

main()