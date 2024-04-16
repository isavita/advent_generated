function main()
    replacements = []
    molecule = ""
    open("input.txt") do file
        for line in eachline(file)
            if isempty(line)
                continue
            elseif occursin(" => ", line)
                push!(replacements, line)
            else
                molecule = line
            end
        end
    end

    molecules = Set{String}()
    for replacement in replacements
        parts = split(replacement, " => ")
        for i in 1:length(molecule)
            if startswith(molecule[i:end], parts[1])
                new_molecule = molecule[1:i-1] * parts[2] * molecule[i+length(parts[1]):end]
                push!(molecules, new_molecule)
            end
        end
    end

    println(length(molecules))
end

main()