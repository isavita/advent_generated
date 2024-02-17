
function evaluate(expression)
    tokens = tokenize(expression)
    return evaluateTokens(tokens)
end

function tokenize(expression)
    expression = replace(expression, "(" => "( ")
    expression = replace(expression, ")" => " )")
    return split(expression)
end

function evaluateTokens(tokens)
    ops = []
    vals = []

    for token in tokens
        if token == "("
            push!(ops, token)
        elseif token in ["+", "*"]
            while length(ops) > 0 && ops[end] != "("
                push!(vals, applyOp(pop!(ops), pop!(vals), pop!(vals)))
            end
            push!(ops, token)
        elseif token == ")"
            while ops[end] != "("
                push!(vals, applyOp(pop!(ops), pop!(vals), pop!(vals)))
            end
            pop!(ops) # Remove the opening '('
        else
            push!(vals, parse(Int, token))
        end
    end

    while length(ops) > 0
        push!(vals, applyOp(pop!(ops), pop!(vals), pop!(vals)))
    end

    return vals[1]
end

function applyOp(op, a, b)
    if op == "+"
        return a + b
    elseif op == "*"
        return a * b
    else
        error("Unknown operator: $op")
    end
end

global sum = 0
for expression in eachline("input.txt")
    result = evaluate(expression)
    global sum += result
end

println(sum)
