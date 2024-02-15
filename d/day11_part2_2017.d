
import std.stdio;
import std.file;
import std.array;
import std.string;

int abs(int x) {
    return x < 0 ? -x : x;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int distance(int x, int y, int z) {
    return (abs(x) + abs(y) + abs(z)) / 2;
}

void main() {
    string input = cast(string) readText("input.txt");

    auto directions = input.split(",");

    int x = 0, y = 0, z = 0;
    int maxDistance = 0;

    foreach (dir; directions) {
        switch (dir) {
            case "n":
                y++;
                z--;
                break;
            case "ne":
                x++;
                z--;
                break;
            case "se":
                x++;
                y--;
                break;
            case "s":
                y--;
                z++;
                break;
            case "sw":
                x--;
                z++;
                break;
            case "nw":
                x--;
                y++;
                break;
            default:
                break;
        }

        int curDistance = distance(x, y, z);
        maxDistance = max(maxDistance, curDistance);
    }

    writeln(maxDistance);
}
