
import std.stdio;
import std.file;
import std.string;

int decode(string pass) {
    int row = binaryToInt(pass[0 .. 7]);
    int column = binaryToInt(pass[7 .. $]);
    return row * 8 + column;
}

int binaryToInt(string binaryStr) {
    int result = 0;
    foreach (i, char c; binaryStr) {  // corrected identifier from 'char' to 'c'
        if (c == '1') {
            result |= 1 << (binaryStr.length - i - 1);
        }
    }
    return result;
}

void main() {
    string input = cast(string)std.file.read("input.txt");
    int maxSeatID = 0;
    foreach (line; input.splitLines()) {
        string pass = line.strip().replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1");
        int seatID = decode(pass);
        if (seatID > maxSeatID) {
            maxSeatID = seatID;
        }
    }
    
    writeln(maxSeatID);
}
