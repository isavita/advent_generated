
function react(polymer)
    for i in 1:length(polymer)-1
        if polymer[i] != polymer[i+1] &&
            (Int(polymer[i]) + 32 == Int(polymer[i+1]) ||
                Int(polymer[i]) - 32 == Int(polymer[i+1]))
            return react(string(polymer[1:i-1], polymer[i+2:end]))
        end
    end
    return polymer
end

file = open("input.txt")
polymer = readline(file)

result = react(polymer)
println(length(result))
