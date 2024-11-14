
function supportsSSL(ip::String)
    bracket_contents = collect(m.match for m in eachmatch(r"\[[a-z]+\]", ip))
    ip = replace(ip, r"\[[a-z]+\]" => "-")
    abas = [ip[i:i+2] for i in 1:(length(ip)-2) if ip[i] != ip[i+1] && ip[i] == ip[i+2]]
    for aba in abas
        bab = string(aba[2], aba[1], aba[2])
        if any(contains(bc, bab) for bc in bracket_contents)
            return true
        end
    end
    return false
end

function main()
    ssl_count = 0
    for line in eachline("input.txt")
        ssl_count += supportsSSL(line) ? 1 : 0
    end
    println(ssl_count)
end

main()
