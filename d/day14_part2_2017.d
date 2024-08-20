import std.stdio;
import std.file;
import std.conv;
import std.string;

string knotHash(string input) {
    int[256] list;
    foreach (i; 0 .. 256) {
        list[i] = i;
    }

    int[] lengths;
    foreach (c; input) {
        lengths ~= cast(int)c;
    }
    lengths ~= [17, 31, 73, 47, 23];

    int pos = 0;
    int skip = 0;
    foreach (_; 0 .. 64) {
        foreach (length; lengths) {
            foreach (i; 0 .. length / 2) {
                int tmp = list[(pos + i) % 256];
                list[(pos + i) % 256] = list[(pos + length - 1 - i) % 256];
                list[(pos + length - 1 - i) % 256] = tmp;
            }
            pos = (pos + length + skip) % 256;
            skip++;
        }
    }

    string denseHash;
    foreach (i; 0 .. 16) {
        int xor = 0;
        foreach (j; 0 .. 16) {
            xor ^= list[i * 16 + j];
        }
        denseHash ~= format("%02x", xor);
    }

    return denseHash;
}

string hexToBinary(string hex) {
    string binary;
    foreach (c; hex) {
        int num = to!int(c.to!string, 16);
        binary ~= format("%04b", num);
    }
    return binary;
}

void dfs(int x, int y, ref bool[][] visited, string[] grid) {
    if (x < 0 || x >= 128 || y < 0 || y >= 128) return;
    if (visited[x][y] || grid[x][y] != '#') return;

    visited[x][y] = true;

    dfs(x + 1, y, visited, grid);
    dfs(x - 1, y, visited, grid);
    dfs(x, y + 1, visited, grid);
    dfs(x, y - 1, visited, grid);
}

void main() {
    auto file = File("input.txt", "r");
    string keyString = file.readln().strip();
    file.close();

    string[] grid;
    foreach (i; 0 .. 128) {
        string hashInput = keyString ~ "-" ~ to!string(i);
        string hash = knotHash(hashInput);
        string binary = hexToBinary(hash);

        string row;
        foreach (b; binary) {
            row ~= (b == '1' ? "#" : ".");
        }
        grid ~= row;
    }

    bool[][] visited;
    visited.length = 128;
    foreach (i; 0 .. 128) {
        visited[i].length = 128;
    }

    int regions = 0;
    foreach (i; 0 .. 128) {
        foreach (j; 0 .. 128) {
            if (!visited[i][j] && grid[i][j] == '#') {
                regions++;
                dfs(i, j, visited, grid);
            }
        }
    }

    writeln(regions);
}