#include <iostream>
#include <fstream>
#include <string>

std::string readInput(const std::string &filename);
std::string findNextPassword(const std::string &password);
std::string incrementPassword(const std::string &password);
bool isValidPassword(const std::string &password);
bool hasStraight(const std::string &password);
bool containsInvalidLetters(const std::string &password);
bool hasTwoPairs(const std::string &password);

int main() {
    std::string currentPassword = readInput("input.txt");

    std::string firstNewPassword = findNextPassword(currentPassword);
    std::string secondNewPassword = findNextPassword(firstNewPassword);
    std::cout << secondNewPassword << std::endl;

    return 0;
}

std::string readInput(const std::string &filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        return "";
    }

    std::string password;
    std::getline(file, password);
    return password;
}

std::string findNextPassword(const std::string &password) {
    std::string newPassword = password;
    do {
        newPassword = incrementPassword(newPassword);
    } while (!isValidPassword(newPassword));

    return newPassword;
}

std::string incrementPassword(const std::string &password) {
    std::string newPassword = password;
    for (int i = newPassword.length() - 1; i >= 0; i--) {
        newPassword[i]++;
        if (newPassword[i] > 'z') {
            newPassword[i] = 'a';
        } else {
            break;
        }
    }
    return newPassword;
}

bool isValidPassword(const std::string &password) {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

bool hasStraight(const std::string &password) {
    for (size_t i = 0; i < password.length() - 2; i++) {
        if (password[i] + 1 == password[i + 1] && password[i] + 2 == password[i + 2]) {
            return true;
        }
    }
    return false;
}

bool containsInvalidLetters(const std::string &password) {
    for (char c : password) {
        if (c == 'i' || c == 'o' || c == 'l') {
            return true;
        }
    }
    return false;
}

bool hasTwoPairs(const std::string &password) {
    int count = 0;
    for (size_t i = 0; i < password.length() - 1; i++) {
        if (password[i] == password[i + 1]) {
            count++;
            i++; // Skip the next character
        }
    }
    return count >= 2;
}