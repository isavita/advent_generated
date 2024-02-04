import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int validPasswordsPart1 = 0;
  int validPasswordsPart2 = 0;
  
  for (String line in lines) {
    List<String> parts = line.split(' ');
    List<String> range = parts[0].split('-');
    int min = int.parse(range[0]);
    int max = int.parse(range[1]);
    String letter = parts[1][0];
    String password = parts[2];
    
    // Part 1
    int count = password.split(letter).length - 1;
    if (count >= min && count <= max) {
      validPasswordsPart1++;
    }
    
    // Part 2
    if ((password[min - 1] == letter && password[max - 1] != letter) || 
        (password[min - 1] != letter && password[max - 1] == letter)) {
      validPasswordsPart2++;
    }
  }
  
  print(validPasswordsPart1);
  print(validPasswordsPart2);
}