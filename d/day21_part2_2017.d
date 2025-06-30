
import std.stdio, std.string, std.file, std.algorithm, std.conv, std.range;

string rotate(string s) {
    auto parts = s.split('/');
    auto size = parts.length;
    auto newParts = new string[size];
    foreach (x; 0 .. size) {
        char[] newRow;
        foreach_reverse (y; 0 .. size) {
            newRow ~= parts[y][x];
        }
        newParts[x] = to!string(newRow);
    }
    return newParts.join("/");
}

string flip(string s) {
    auto parts = s.split('/');
    foreach (ref p; parts) {
        p = to!string(retro(p));
    }
    return parts.join("/");
}

void main() {
    string[string] rules;
    foreach (line; readText("input.txt").strip.split("\n")) {
        auto parts = line.split(" => ");
        auto input = parts[0], output = parts[1];
        
        auto current = input;
        foreach (i; 0 .. 4) {
            rules[current] = output;
            current = rotate(current);
        }
        
        current = flip(input);
        foreach (i; 0 .. 4) {
            rules[current] = output;
            current = rotate(current);
        }
    }

    auto grid = [".#.", "..#", "###"];

    foreach (i; 0 .. 18) {
        size_t subSize = (grid.length % 2 == 0) ? 2 : 3;
        size_t newSize = grid.length / subSize * (subSize + 1);
        auto newGrid = new string[newSize];

        foreach (y; 0 .. grid.length / subSize) {
            foreach (x; 0 .. grid.length / subSize) {
                string[] square;
                foreach (dy; 0 .. subSize) {
                    square ~= grid[y * subSize + dy][x * subSize .. x * subSize + subSize];
                }
                
                auto newSquareRows = rules[square.join("/")].split("/");
                
                foreach (dy; 0 .. newSquareRows.length) {
                    newGrid[y * (subSize + 1) + dy] ~= newSquareRows[dy];
                }
            }
        }
        grid = newGrid;
    }

    writeln(grid.joiner.count('#'));
}
