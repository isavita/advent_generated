import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int totalSquareFeet = 0;
  
  for (String line in lines) {
    List<int> dimensions = line.split('x').map(int.parse).toList();
    int l = dimensions[0];
    int w = dimensions[1];
    int h = dimensions[2];
    
    int side1 = l * w;
    int side2 = w * h;
    int side3 = h * l;
    
    int extra = [side1, side2, side3].reduce((a, b) => a < b ? a : b);
    
    totalSquareFeet += 2 * side1 + 2 * side2 + 2 * side3 + extra;
  }
  
  print(totalSquareFeet);
}