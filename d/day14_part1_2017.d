import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;
import std.format;

string knotHash(string input) {
    int[] lengths;
    foreach (char c; input) {
        lengths ~= cast(int)c;
    }
    lengths ~= [17, 31, 73, 47, 23];

    int[] list = new int[256];
    foreach (i; 0 .. 256) {
        list[i] = i;
    }

    int currentPosition = 0;
    int skipSize = 0;

    foreach (_; 0 .. 64) {
        foreach (length; lengths) {
            foreach (i; 0 .. length / 2) {
                int a = (currentPosition + i) % 256;
                int b = (currentPosition + length - 1 - i) % 256;
                int temp = list[a];
                list[a] = list[b];
                list[b] = temp;
            }
            currentPosition = (currentPosition + length + skipSize) % 256;
            skipSize++;
        }
    }

    string hash = "";
    foreach (i; 0 .. 16) {
        int xorValue = list[i * 16];
        foreach (j; 1 .. 16) {
            xorValue ^= list[i * 16 + j];
        }
        hash ~= format("%02x", xorValue);
    }
    return hash;
}

string hexToBinary(string hex) {
    string binary = "";
    foreach (char c; hex) {
        int value = to!int(c.to!string, 16);
        foreach_reverse (i; 0 .. 4) {
            binary ~= ((value >> i) & 1) ? "1" : "0";
        }
    }
    return binary;
}

void main() {
    string keyString = cast(string)readText("input.txt").idup.strip();

    int usedSquares = 0;
    foreach (i; 0 .. 128) {
        string hashInput = keyString ~ "-" ~ to!string(i);
        string hash = knotHash(hashInput);
        string binary = hexToBinary(hash);
        foreach (char b; binary) {
            if (b == '1') {
                usedSquares++;
            }
        }
    }

    writeln(usedSquares);
}