import 'dart:io';

void main() {
  var currentPassword = readInput("input.txt");
  var firstNewPassword = findNextPassword(currentPassword);
  var secondNewPassword = findNextPassword(firstNewPassword);
  print(secondNewPassword);
}

String readInput(String filename) {
  var file = File(filename);
  var text = file.readAsStringSync().trim();
  return text;
}

String findNextPassword(String password) {
  while (true) {
    password = incrementPassword(password);
    if (isValidPassword(password)) {
      break;
    }
  }
  return password;
}

String incrementPassword(String password) {
  var runes = password.runes.toList();
  for (var i = runes.length - 1; i >= 0; i--) {
    runes[i]++;
    if (runes[i] > 122) {
      runes[i] = 97;
    } else {
      break;
    }
  }
  return String.fromCharCodes(runes);
}

bool isValidPassword(String password) {
  return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

bool hasStraight(String password) {
  for (var i = 0; i < password.length - 2; i++) {
    if (password.codeUnitAt(i) + 1 == password.codeUnitAt(i + 1) && password.codeUnitAt(i) + 2 == password.codeUnitAt(i + 2)) {
      return true;
    }
  }
  return false;
}

bool containsInvalidLetters(String password) {
  for (var c in password.runes) {
    if (c == 105 || c == 111 || c == 108) {
      return true;
    }
  }
  return false;
}

bool hasTwoPairs(String password) {
  var count = 0;
  for (var i = 0; i < password.length - 1; i++) {
    if (password[i] == password[i + 1]) {
      count++;
      i++; // Skip the next character
    }
  }
  return count >= 2;
}