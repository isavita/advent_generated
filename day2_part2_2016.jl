
instructions = readlines("input.txt")

function getBathroomCode(instructions)
    keypad = Dict(
        "1" => Dict('D' => "3"),
        "2" => Dict('R' => "3", 'D' => "6"),
        "3" => Dict('U' => "1", 'R' => "4", 'D' => "7", 'L' => "2"),
        "4" => Dict('L' => "3", 'D' => "8"),
        "5" => Dict('R' => "6"),
        "6" => Dict('U' => "2", 'R' => "7", 'D' => "A", 'L' => "5"),
        "7" => Dict('U' => "3", 'R' => "8", 'D' => "B", 'L' => "6"),
        "8" => Dict('U' => "4", 'R' => "9", 'D' => "C", 'L' => "7"),
        "9" => Dict('L' => "8"),
        "A" => Dict('U' => "6", 'R' => "B"),
        "B" => Dict('U' => "7", 'R' => "C", 'D' => "D", 'L' => "A"),
        "C" => Dict('U' => "8", 'L' => "B"),
        "D" => Dict('U' => "B")
    )
    position = "5"
    code = ""

    for instruction in instructions
        for move in instruction
            if haskey(keypad[position], move)
                position = keypad[position][move]
            end
        end
        code *= position
    end

    return code
end

code = getBathroomCode(instructions)
println(code)
