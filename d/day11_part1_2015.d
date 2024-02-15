
import std.stdio;
import std.file;

string readInput(string filename) {
    return cast(string)std.file.read(filename).idup;
}

string findNextPassword(string password) {
    while (true) {
        password = incrementPassword(password);
        if (isValidPassword(password)) {
            break;
        }
    }
    return password;
}

string incrementPassword(string password) {
    char[] runes = password.dup;
    for (int i = cast(int)(password.length - 1); i >= 0; i--) {
        runes[i]++;
        if (runes[i] > 'z') {
            runes[i] = 'a';
        } else {
            break;
        }
    }
    return runes.idup;
}

bool isValidPassword(string password) {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

bool hasStraight(string password) {
    for (int i = 0; i < cast(int)(password.length - 2); i++) {
        if (password[i] + 1 == password[i + 1] && password[i] + 2 == password[i + 2]) {
            return true;
        }
    }
    return false;
}

bool containsInvalidLetters(string password) {
    foreach (char c; password) {
        if (c == 'i' || c == 'o' || c == 'l') {
            return true;
        }
    }
    return false;
}

bool hasTwoPairs(string password) {
    int count = 0;
    for (int i = 0; i < cast(int)(password.length - 1); i++) {
        if (password[i] == password[i + 1]) {
            count++;
            i++; // Skip the next character
        }
    }
    return count >= 2;
}

void main() {
    string currentPassword = readInput("input.txt");
    string newPassword = findNextPassword(currentPassword);
    writeln(newPassword);
}
