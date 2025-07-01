
import std.stdio;
import std.file;

uint nextState(uint grid) {
    uint newGrid = 0;
    const int Side = 5;
    const int Square = 25;

    for (int i = 0; i < Square; ++i) {
        int row = i / Side;
        int col = i % Side;
        int neighbors = 0;

        if (row > 0 && (grid & (1u << (i - Side))) != 0) neighbors++;
        if (row < Side - 1 && (grid & (1u << (i + Side))) != 0) neighbors++;
        if (col > 0 && (grid & (1u << (i - 1))) != 0) neighbors++;
        if (col < Side - 1 && (grid & (1u << (i + 1))) != 0) neighbors++;

        bool isBug = (grid & (1u << i)) != 0;

        if ((isBug && neighbors == 1) || (!isBug && (neighbors == 1 || neighbors == 2))) {
            newGrid |= (1u << i);
        }
    }
    return newGrid;
}

void main() {
    uint grid = 0;
    int i = 0;
    foreach (char c; readText("input.txt")) {
        if (c == '#') {
            grid |= (1u << i);
        }
        if (c == '#' || c == '.') {
            i++;
        }
    }

    bool[uint] appeared;
    while (grid !in appeared) {
        appeared[grid] = true;
        grid = nextState(grid);
    }

    writeln(grid);
}
