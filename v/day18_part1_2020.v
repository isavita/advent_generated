
import os

fn tokenize(expression string) []string {
    return expression.replace_each([ '(', '( ', ')', ' ) ' ]).split(' ').filter(it != '')
}

fn apply_op(op byte, a i64, b i64) i64 {
    return if op == `+` { a + b } else { a * b }
}

fn evaluate_tokens(tokens []string) i64 {
    mut ops := []byte{}
    mut vals := []i64{}
    for token in tokens {
        match token {
            '(' { ops << `(` }
            '+' {
                for ops.len > 0 && ops.last() != `(` {
                    b := vals.pop()
                    a := vals.pop()
                    vals << apply_op(ops.pop(), a, b)
                }
                ops << `+`
            }
            '*' {
                for ops.len > 0 && ops.last() != `(` {
                    b := vals.pop()
                    a := vals.pop()
                    vals << apply_op(ops.pop(), a, b)
                }
                ops << `*`
            }
            ')' {
                for ops.last() != `(` {
                    b := vals.pop()
                    a := vals.pop()
                    vals << apply_op(ops.pop(), a, b)
                }
                ops.pop()
            }
            else { vals << token.i64() }
        }
    }
    for ops.len > 0 {
        b := vals.pop()
        a := vals.pop()
        vals << apply_op(ops.pop(), a, b)
    }
    return vals[0]
}

fn main() {
    data := os.read_file('input.txt') or { panic(err) }
    mut sum := i64(0)
    for line in data.split_into_lines() {
        sum += evaluate_tokens(tokenize(line))
    }
    println(sum)
}
