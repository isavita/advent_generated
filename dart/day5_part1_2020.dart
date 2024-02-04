
import 'dart:io';

void main() {
  List<String> boardingPasses = File('input.txt').readAsLinesSync();
  
  int maxSeatID = 0;
  
  for (String boardingPass in boardingPasses) {
    int row = int.parse(boardingPass.substring(0, 7).replaceAll('F', '0').replaceAll('B', '1'), radix: 2);
    int column = int.parse(boardingPass.substring(7).replaceAll('L', '0').replaceAll('R', '1'), radix: 2);
    
    int seatID = row * 8 + column;
    
    if (seatID > maxSeatID) {
      maxSeatID = seatID;
    }
  }
  
  print(maxSeatID);
}
