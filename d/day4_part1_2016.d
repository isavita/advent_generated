
import std.stdio;
import std.file;
import std.algorithm;
import std.conv;
import std.range;
import std.string;
import std.typecons;

struct LetterCount {
    char letter;
    int count;
}

void main() {
    auto file = File("input.txt", "r");

    int sumOfSectorIDs = 0;
    foreach (line; file.byLine()) {
        if (isRealRoom(line.idup)) {
            sumOfSectorIDs += getSectorID(line.idup);
        }
    }

    writeln(sumOfSectorIDs);
}

bool isRealRoom(string room) {
    auto parts = room.split("[");
    string checksum = parts[1].stripRight("]");
    auto encryptedName = parts[0].split("-");
    encryptedName.popBack();

    LetterCount[] counts;
    foreach (part; encryptedName) {
        foreach (letter; part) {
            auto lc = counts.find!(a => a.letter == letter);
            if (lc.empty) {
                counts ~= LetterCount(letter, 1);
            } else {
                lc.front.count++;
            }
        }
    }

    counts.sort!((a, b) {
        return a.count == b.count ? a.letter < b.letter : a.count > b.count;
    });

    foreach (i, c; checksum) {
        if (cast(char)c != counts[i].letter) {
            return false;
        }
    }

    return true;
}

int getSectorID(string room) {
    auto parts = room.split("-");
    string sectorIDPart = parts[$-1];
    return to!int(sectorIDPart.split("[")[0]);
}
