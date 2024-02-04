
import 'dart:io';

void main() {
  List<int> banks = File('input.txt').readAsStringSync().trim().split('\t').map(int.parse).toList();
  
  List<String> configurations = [];
  int cycles = 0;
  
  while(!configurations.contains(banks.join(' '))) {
    configurations.add(banks.join(' '));
    
    int maxBlocks = banks.reduce((value, element) => value > element ? value : element);
    int index = banks.indexOf(maxBlocks);
    
    banks[index] = 0;
    
    for(int i = 1; i <= maxBlocks; i++) {
      banks[(index + i) % banks.length]++;
    }
    
    cycles++;
  }
  
  print(cycles);
}
