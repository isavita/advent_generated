
import 'dart:io';

void main() {
  List<String> ips = File('input.txt').readAsLinesSync();
  
  int tlsCount = 0;
  int sslCount = 0;
  
  for (String ip in ips) {
    List<String> supernetSequences = [];
    List<String> hypernetSequences = [];
    
    String currentSequence = "";
    bool inHypernet = false;
    
    for (int i = 0; i < ip.length; i++) {
      if (ip[i] == '[') {
        supernetSequences.add(currentSequence);
        currentSequence = "";
        inHypernet = true;
      } else if (ip[i] == ']') {
        hypernetSequences.add(currentSequence);
        currentSequence = "";
        inHypernet = false;
      } else {
        currentSequence += ip[i];
      }
      
      if (i == ip.length - 1) {
        if (inHypernet) {
          hypernetSequences.add(currentSequence);
        } else {
          supernetSequences.add(currentSequence);
        }
      }
    }
    
    bool supportsTLS = false;
    
    for (String sequence in supernetSequences) {
      if (hasABBA(sequence)) {
        supportsTLS = true;
        break;
      }
    }
    
    if (supportsTLS) {
      for (String sequence in hypernetSequences) {
        if (hasABBA(sequence)) {
          supportsTLS = false;
          break;
        }
      }
    }
    
    if (supportsTLS) {
      tlsCount++;
    }
    
    bool supportsSSL = false;
    
    for (String sequence in supernetSequences) {
      List<String> abas = getABAs(sequence);
      for (String aba in abas) {
        String bab = aba[1] + aba[0] + aba[1];
        for (String hypernet in hypernetSequences) {
          if (hypernet.contains(bab)) {
            supportsSSL = true;
            break;
          }
        }
        if (supportsSSL) {
          break;
        }
      }
      if (supportsSSL) {
        break;
      }
    }
    
    if (supportsSSL) {
      sslCount++;
    }
  }
  
  print(tlsCount);
  print(sslCount);
}

bool hasABBA(String sequence) {
  for (int i = 0; i < sequence.length - 3; i++) {
    if (sequence[i] != sequence[i + 1] && sequence[i] == sequence[i + 3] && sequence[i + 1] == sequence[i + 2]) {
      return true;
    }
  }
  return false;
}

List<String> getABAs(String sequence) {
  List<String> abas = [];
  for (int i = 0; i < sequence.length - 2; i++) {
    if (sequence[i] != sequence[i + 1] && sequence[i] == sequence[i + 2]) {
      abas.add(sequence.substring(i, i + 3));
    }
  }
  return abas;
}
