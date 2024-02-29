#!/bin/bash

input_file="input.txt"

if [ ! -f "$input_file" ]; then
    echo "Error: File '$input_file' not found."
    exit 1
fi

awk -v input=1 '
BEGIN {
    PROGRAM_SIZE = 10000
    for (i = 0; i < PROGRAM_SIZE; ++i) {
        program[i] = 0
    }
}

{
    split($0, strProgram, /,/)
    for (i = 1; i <= length(strProgram); ++i) {
        program[i - 1] = strProgram[i]
    }
}

function getMode(instruction, position) {
    return int(instruction / (10 ^ (position + 1)) % 10)
}

function getParam(pointer, mode) {
    if (mode == 0) {
        return program[program[pointer]]
    }
    return program[pointer]
}

function runProgram() {
    output = 0
    pointer = 0
    while (pointer < PROGRAM_SIZE) {
        instruction = program[pointer]
        opcode = instruction % 100

        if (opcode == 1 || opcode == 2) {
            param1 = getParam(pointer + 1, getMode(instruction, 1))
            param2 = getParam(pointer + 2, getMode(instruction, 2))
            result = (opcode == 1) ? param1 + param2 : param1 * param2
            program[program[pointer + 3]] = result
            pointer += 4
        } else if (opcode == 3) {
            program[program[pointer + 1]] = input
            pointer += 2
        } else if (opcode == 4) {
            output = getParam(pointer + 1, getMode(instruction, 1))
            pointer += 2
        } else if (opcode == 99) {
            return output
        } else {
            print "Unknown opcode: " opcode
            exit 1
        }
    }
    return output
}

END {
    print runProgram()
}' "$input_file"