
using DelimitedFiles

function main()
    input = chomp(readdlm("input.txt", String)[1])
    halfway = div(length(input), 2)
    sum = 0

    for i in 1:length(input)
        next = mod1(i + halfway, length(input))
        if input[i] == input[next]
            sum += Int(input[i] - '0')
        end
    end

    println(sum)
end

main()
