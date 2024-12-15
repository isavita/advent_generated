
function look_and_say(input::String, iterations::Int)
    seq = collect(input)
    for _ in 1:iterations
        new_seq = Char[]
        i = 1
        while i <= length(seq)
            j = i
            while j <= length(seq) && seq[i] == seq[j]
                j += 1
            end
            push!(new_seq, Char(j - i + '0'))
            push!(new_seq, seq[i])
            i = j
        end
        seq = new_seq
    end
    return length(seq)
end

function main()
    input = open("input.txt", "r") do f
        readline(f)
    end
    result = look_and_say(input, 40)
    println(result)
end

main()
