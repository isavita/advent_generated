
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.regex;
import std.algorithm;

struct Action {
    int write;
    int move;
    char nextState;
}

struct State {
    Action[2] rules;
}

void main() {
    auto blocks = readText("input.txt").strip().split("\n\n");

    auto header = blocks[0];
    auto initialState = matchFirst(header, r"Begin in state (\w).")[1][0];
    auto diagnosticChecksum = to!long(matchFirst(header, r"(\d+) steps")[1]);

    State[char] states;
    foreach (block; blocks[1 .. $]) {
        auto lines = block.split("\n");
        auto stateName = matchFirst(lines[0], r"In state (\w):")[1][0];

        auto write0 = to!int(matchFirst(lines[2], r"value (\d)")[1]);
        auto move0 = lines[3].canFind("right") ? 1 : -1;
        auto nextState0 = matchFirst(lines[4], r"state (\w)")[1][0];

        auto write1 = to!int(matchFirst(lines[6], r"value (\d)")[1]);
        auto move1 = lines[7].canFind("right") ? 1 : -1;
        auto nextState1 = matchFirst(lines[8], r"state (\w)")[1][0];

        states[stateName] = State([Action(write0, move0, nextState0), Action(write1, move1, nextState1)]);
    }

    long cursor = 0;
    char state = initialState;
    int[long] tape;

    foreach (i; 0 .. diagnosticChecksum) {
        int value = tape.get(cursor, 0);
        auto rule = states[state].rules[value];
        tape[cursor] = rule.write;
        cursor += rule.move;
        state = rule.nextState;
    }

    writeln(tape.values.sum());
}
