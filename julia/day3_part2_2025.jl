
function main()
    target, total = 12, big"0"
    stack = UInt8[]
    sizehint!(stack, 10000)
    
    if isfile("input.txt")
        for line in eachline("input.txt")
            s = rstrip(c -> !isdigit(c), line)
            n = length(s)
            if n >= target
                rem = n - target
                empty!(stack)
                for b in codeunits(s)
                    while rem > 0 && !isempty(stack) && stack[end] < b
                        pop!(stack)
                        rem -= 1
                    end
                    push!(stack, b)
                end
                total += parse(BigInt, String(stack[1:target]))
            end
        end
    end
    println(total)
end

main()
