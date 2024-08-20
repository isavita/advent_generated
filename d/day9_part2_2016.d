import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string; // Added import for std.string

long decompressLength(string data, bool versionTwo) {
    long length = 0;
    size_t i = 0;
    while (i < data.length) {
        if (data[i] == '(') {
            auto endMarker = data.indexOf(')', i);
            auto marker = data[i + 1 .. endMarker];
            auto parts = marker.split('x');
            auto chars = to!int(parts[0]);
            auto repeats = to!int(parts[1]);
            i = endMarker + 1;
            if (versionTwo) {
                length += repeats * decompressLength(data[i .. i + chars], versionTwo);
            } else {
                length += repeats * chars;
            }
            i += chars;
        } else {
            length++;
            i++;
        }
    }
    return length;
}

void main() {
    auto data = cast(string) read("input.txt");
    data = data.replace("\n", "").replace(" ", "");
    writeln(decompressLength(data, true));
}