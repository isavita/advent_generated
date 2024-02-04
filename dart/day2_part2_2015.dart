import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int totalWrappingPaper = 0;
  int totalRibbon = 0;
  
  for (String line in lines) {
    List<int> dimensions = line.split('x').map(int.parse).toList();
    
    int l = dimensions[0];
    int w = dimensions[1];
    int h = dimensions[2];
    
    int side1 = l * w;
    int side2 = w * h;
    int side3 = h * l;
    
    int smallestSide = [side1, side2, side3].reduce((a, b) => a < b ? a : b);
    
    int surfaceArea = 2 * side1 + 2 * side2 + 2 * side3;
    
    totalWrappingPaper += surfaceArea + smallestSide;
    
    List<int> sortedDimensions = [l, w, h]..sort();
    int ribbonLength = 2 * sortedDimensions[0] + 2 * sortedDimensions[1];
    int bowLength = l * w * h;
    
    totalRibbon += ribbonLength + bowLength;
  }
  
  print(totalWrappingPaper);
  print(totalRibbon);
}