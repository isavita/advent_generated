
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  Set<String> programsAbove = Set();
  Set<String> programsBelow = Set();
  
  for (String line in lines) {
    List<String> parts = line.split(' -> ');
    String program = parts[0].split(' ')[0];
    programsBelow.add(program);
    
    if (parts.length > 1) {
      List<String> above = parts[1].split(', ');
      programsAbove.addAll(above);
    }
  }
  
  String bottomProgram = programsBelow.difference(programsAbove).first;
  
  print(bottomProgram);
}
