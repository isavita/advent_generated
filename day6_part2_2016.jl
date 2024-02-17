
function get_original_message(messages)
    if length(messages) == 0
        return ""
    end
    
    message_length = length(messages[1])
    count = [Dict{Char, Int}() for _ in 1:message_length]
    
    for message in messages
        for (j, char) in enumerate(message)
            count[j][char] = get(count[j], char, 0) + 1
        end
    end
    
    original_message = ""
    for char_count in count
        original_message *= get_least_common_char(char_count)
    end
    
    return original_message
end

function get_least_common_char(count)
    min_char = ' '
    min_count = typemax(Int)
    
    for (char, cnt) in count
        if cnt < min_count
            min_count = cnt
            min_char = char
        end
    end
    
    return min_char
end

messages = readlines("input.txt")
original_message = get_original_message(messages)
println(original_message)
