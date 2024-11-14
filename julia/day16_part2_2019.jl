
function read_input(filename)
    open(filename, "r") do file
        return chomp(readline(file))
    end
end

function repeat_input(input, times)
    digits = parse.(Int, collect(repeat(input, times)))
end

function fft!(signal, phases, offset)
    for _ in 1:phases
        sum = 0
        for i in length(signal):-1:offset+1
            sum += signal[i]
            signal[i] = sum % 10
        end
    end
end

function main()
    input = read_input("input.txt")
    offset = parse(Int, input[1:7])
    signal = repeat_input(input, 10000)
    fft!(signal, 100, offset)
    println(join(signal[offset+1:offset+8]))
end

main()
