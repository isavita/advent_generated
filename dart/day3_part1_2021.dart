
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  String gammaRate = '';
  String epsilonRate = '';
  
  for (int i = 0; i < lines[0].length; i++) {
    int countZero = 0;
    int countOne = 0;
    
    for (String line in lines) {
      if (line[i] == '0') {
        countZero++;
      } else {
        countOne++;
      }
    }
    
    if (countZero > countOne) {
      gammaRate += '0';
    } else {
      gammaRate += '1';
    }
    
    if (countZero < countOne) {
      epsilonRate += '0';
    } else {
      epsilonRate += '1';
    }
  }
  
  int gammaRateDecimal = int.parse(gammaRate, radix: 2);
  int epsilonRateDecimal = int.parse(epsilonRate, radix: 2);
  
  int powerConsumption = gammaRateDecimal * epsilonRateDecimal;
  
  print(powerConsumption);
}
