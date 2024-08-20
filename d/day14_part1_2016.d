import std.stdio;
import std.file;
import std.string;
import std.digest.md;
import std.conv;

string md5(string input) {
    auto digest = md5Of(input);
    return digest.toHexString.idup;
}

string findTriplet(string hash) {
    foreach (i; 0 .. hash.length - 2) {
        if (hash[i] == hash[i+1] && hash[i] == hash[i+2]) {
            return hash[i..i+1];
        }
    }
    return "";
}

bool hasQuintuplet(string hash, char c) {
    foreach (i; 0 .. hash.length - 4) {
        if (hash[i] == c && hash[i+1] == c && hash[i+2] == c && hash[i+3] == c && hash[i+4] == c) {
            return true;
        }
    }
    return false;
}

void main() {
    string salt;
    try {
        salt = cast(string) readText("input.txt");
    } catch (Exception e) {
        writeln("Error reading input.txt");
        return;
    }

    int keysFound = 0;
    int index = 0;
    string[1001] futureHashes;

    foreach (i; 0 .. 1001) {
        futureHashes[i] = md5(salt ~ to!string(i));
    }

    while (keysFound < 64) {
        string currentHash = futureHashes[0];

        foreach (i; 0 .. 1000) {
            futureHashes[i] = futureHashes[i+1];
        }
        futureHashes[1000] = md5(salt ~ to!string(index + 1001));

        string triplet = findTriplet(currentHash);
        if (triplet.length > 0) {
            foreach (i; 1 .. 1001) {
                if (hasQuintuplet(futureHashes[i], triplet[0])) {
                    keysFound++;
                    break;
                }
            }
        }
        index++;
    }

    writeln(index - 1);
}