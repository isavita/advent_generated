
#!/usr/bin/awk -f
function push_num(v) { num_top++; num_stack[num_top] = v }
function pop_num() { v = num_stack[num_top]; delete num_stack[num_top]; num_top--; return v }
function push_op(o)  { op_top++; op_stack[op_top] = o }
function pop_op()    { o = op_stack[op_top]; delete op_stack[op_top]; op_top--; return o }
function peek_op()     { return (op_top >= 0) ? op_stack[op_top] : "" }
function precedence(op, rule) {
    if (op == "(") return 0
    if (rule == 1) {
        if (op == "+" || op == "*") return 1
        return 0
    } else {
        if (op == "+") return 2
        if (op == "*") return 1
    }
    return 0
}
function apply_op() {
    op = pop_op()
    right = pop_num()
    left  = pop_num()
    if (op == "+") res = left + right
    else if (op == "*") res = left * right
    else { print "Error: Unknown operator " op > "/dev/stderr"; exit 1 }
    push_num(res)
}
function evaluate(expression, rule,   i, len, c, j, num) {
    num_top = -1
    op_top  = -1
    len = length(expression)
    i = 1
    while (i <= len) {
        c = substr(expression, i, 1)
        if (c ~ /[ \t\r]/) { i++; continue }
        if (c ~ /[0-9]/) {
            num = 0
            j = i
            while (j <= len && substr(expression, j, 1) ~ /[0-9]/) {
                num = num * 10 + (substr(expression, j, 1) + 0)
                j++
            }
            push_num(num)
            i = j
        } else if (c == "(") {
            push_op("("); i++
        } else if (c == ")") {
            while (op_top >= 0 && peek_op() != "(") { apply_op() }
            if (op_top < 0 || peek_op() != "(") { print "Error: Mismatched parentheses" > "/dev/stderr"; exit 1 }
            pop_op(); i++
        } else if (c == "+" || c == "*") {
            while (op_top >= 0 && peek_op() != "(" && precedence(peek_op(), rule) >= precedence(c, rule)) {
                apply_op()
            }
            push_op(c); i++
        } else {
            print "Error: Invalid character " c > "/dev/stderr"; exit 1
        }
    }
    while (op_top >= 0) {
        if (peek_op() == "(") { print "Error: Mismatched parentheses at end" > "/dev/stderr"; exit 1 }
        apply_op()
    }
    if (num_top != 0) { print "Error: Invalid final number stack state" > "/dev/stderr"; exit 1 }
    return pop_num()
}
BEGIN {
    fname = "input.txt"
    total1 = 0
    total2 = 0
    while ((getline line < fname) > 0) {
        gsub(/\r$/, "", line)
        if (length(line) > 0) {
            total1 += evaluate(line, 1)
            total2 += evaluate(line, 2)
        }
    }
    close(fname)
    print total1
    print total2
    exit
}
