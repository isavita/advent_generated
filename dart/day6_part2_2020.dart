import 'dart:io';

void main() {
  List<String> groups = File('input.txt').readAsStringSync().split('\n\n');

  int sumPart1 = 0;
  int sumPart2 = 0;

  for (String group in groups) {
    List<String> answers = group.split('\n');
    Set<String> uniqueAnswers = answers.join().split('').toSet();

    sumPart1 += uniqueAnswers.length;

    Map<String, int> answerCount = {};
    for (String answer in answers.join().split('')) {
      answerCount[answer] = (answerCount[answer] ?? 0) + 1;
    }

    sumPart2 += answerCount.values.where((count) => count == answers.length).length;
  }

  print(sumPart1);
  print(sumPart2);
}