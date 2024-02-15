import std.stdio;
import std.file;
import std.conv;
import std.algorithm;
import std.array;
import std.string;

bool isRealRoom(string room) {
    auto parts = room.split("[");
    string checksum = parts[1].stripRight("]");
    auto encryptedName = parts[0].split("-");
    encryptedName = encryptedName[0 .. $-1];

    int[char] letterCounts;
    foreach (part; encryptedName) {
        foreach (letter; part) {
            letterCounts[letter]++;
        }
    }

    struct LetterCount {
        char letter;
        int count;
    }

    LetterCount[] counts;
    foreach (letter, count; letterCounts) {
        counts ~= LetterCount(letter, count);
    }

    counts.sort!((a, b) {
        if (a.count == b.count) {
            return a.letter < b.letter;
        }
        return a.count > b.count;
    });

    foreach (i, c; checksum) {
        if (c != counts[i].letter) {
            return false;
        }
    }

    return true;
}

int getSectorID(string room) {
    auto parts = room.split("-");
    string sectorIDPart = parts[$-1];
    int sectorID = to!int(sectorIDPart.split("[")[0]);
    return sectorID;
}

string decryptName(string room) {
    auto parts = room.split("-");
    string sectorIDPart = parts[$-1];
    int sectorID = to!int(sectorIDPart.split("[")[0]);
    string decryptedName;

    foreach (part; parts[0 .. $-1]) {
        foreach (letter; part) {
            if (letter == '-') {
                decryptedName ~= ' ';
            } else {
                char shiftedLetter = 'a' + ((letter - 'a' + sectorID) % 26);
                decryptedName ~= cast(char)shiftedLetter;
            }
        }
        decryptedName ~= ' ';
    }

    return decryptedName.strip;
}

void main() {
    string input = cast(string)std.file.read("input.txt");
    auto lines = input.splitLines();

    foreach (line; lines) {
        if (isRealRoom(line)) {
            string decryptedName = decryptName(line);
            if (decryptedName.canFind("northpole object")) {
                writeln(getSectorID(line));
                break;
            }
        }
    }
}