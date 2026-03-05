
BEGIN {
    while ((getline line < "input.txt") > 0) s = s line
    len = length(s)
    
    t1 = 0
    temp = s
    while (match(temp, /-?[0-9]+/)) {
        t1 += substr(temp, RSTART, RLENGTH)
        temp = substr(temp, RSTART + RLENGTH)
    }
    print t1

    pos = 1
    print parse_val()
}

function skip() {
    while (pos <= len && substr(s, pos, 1) ~ /[ \t\n\r]/) pos++
}

function parse_str(  start, v) {
    pos++
    start = pos
    while (pos <= len) {
        if (substr(s, pos, 1) == "\\") pos += 2
        else if (substr(s, pos, 1) == "\"") break
        else pos++
    }
    v = substr(s, start, pos - start)
    pos++
    return v
}

function parse_val(  char, n) {
    skip()
    char = substr(s, pos, 1)
    if (char == "{") return parse_obj()
    if (char == "[") return parse_arr()
    if (char == "\"") { parse_str(); return 0 }
    if (match(substr(s, pos), /^-?[0-9]+/)) {
        n = substr(s, pos, RLENGTH)
        pos += RLENGTH
        return n + 0
    }
    match(substr(s, pos), /^[a-z]+/)
    pos += RLENGTH
    return 0
}

function parse_arr(  sum) {
    pos++
    sum = 0
    while (1) {
        skip()
        if (substr(s, pos, 1) == "]") { pos++; break }
        sum += parse_val()
        skip()
        if (substr(s, pos, 1) == ",") pos++
    }
    return sum
}

function parse_obj(  sum, has_red, v) {
    pos++
    sum = 0
    has_red = 0
    while (1) {
        skip()
        if (substr(s, pos, 1) == "}") { pos++; break }
        parse_str()
        skip()
        pos++ 
        skip()
        if (substr(s, pos, 1) == "\"") {
            v = parse_str()
            if (v == "red") has_red = 1
        } else {
            sum += parse_val()
        }
        skip()
        if (substr(s, pos, 1) == ",") pos++
    }
    return (has_red ? 0 : sum)
}
