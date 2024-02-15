
import std.stdio, std.string, std.conv, std.file;

bool isValidPassword(int password) {
    string s = to!string(password);
    bool hasDouble = false;

    foreach (i; 0 .. s.length - 1) {
        if (s[i] > s[i + 1]) {
            return false;
        }
        if (s[i] == s[i + 1]) {
            if ((i == 0 || s[i] != s[i - 1]) && (i + 2 >= s.length || s[i] != s[i + 2])) {
                hasDouble = true;
            }
        }
    }

    return hasDouble;
}

void main() {
    string rangeStr = cast(string)std.file.readText("input.txt").idup.strip();
    string[] ranges = rangeStr.split("-");
    int start = ranges[0].to!int;
    int end = ranges[1].to!int;

    int count = 0;
    foreach (i; start .. end + 1) {
        if (isValidPassword(i)) {
            count++;
        }
    }

    writeln(count);
}
