import os

fn main() {
    input_file := os.read_file('input.txt') or { panic(err) }
    lines := input_file.split('\n').filter(it.len > 0)

    mut total_code_chars := 0
    mut total_memory_chars := 0
    mut total_encoded_chars := 0

    for line in lines {
        code_length := line.len
        memory_length := decode_string(line).len
        encoded_length := encode_string(line).len

        total_code_chars += code_length
        total_memory_chars += memory_length
        total_encoded_chars += encoded_length
    }

    println('Part 1: ${total_code_chars - total_memory_chars}')
    println('Part 2: ${total_encoded_chars - total_code_chars}')
}

fn decode_string(s string) string {
    return s[1..s.len - 1].replace('\\\\"', '\\').replace('\\"', '"').replace('\x27', '\'')
}

fn encode_string(s string) string {
    return '"${s.replace('\\', '\\\\').replace('"', '\\"')}"'
}