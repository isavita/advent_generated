
import 'dart:io';

List<List<int>> parseInput(List<String> input) {
  List<List<int>> histories = [];
  for (String line in input) {
    List<int> numbers = parseStringToInts(line);
    histories.add(numbers);
  }
  return histories;
}

List<int> parseStringToInts(String numbersLine) {
  List<int> numbers = [];
  List<String> numbersParts = numbersLine.split(' ');
  for (String numberStr in numbersParts) {
    int number = int.parse(numberStr);
    numbers.add(number);
  }
  return numbers;
}

bool allZeros(List<int> nums) {
  for (int num in nums) {
    if (num != 0) {
      return false;
    }
  }
  return true;
}

List<int> calculateExtrapolation(List<int> history) {
  List<int> extrapolations = [];
  for (int i = 1; i < history.length; i++) {
    int extrapolation = history[i] - history[i - 1];
    extrapolations.add(extrapolation);
  }
  return extrapolations;
}

List<List<int>> calculateExtrapolations(List<int> history) {
  List<List<int>> extrapolationsSeries = [];
  extrapolationsSeries.add(history);

  for (int i = 1; i < history.length; i++) {
    List<int> previousExtrapolations = extrapolationsSeries[i - 1];
    if (allZeros(previousExtrapolations)) {
      return extrapolationsSeries;
    }

    List<int> extrapolations = calculateExtrapolation(previousExtrapolations);
    extrapolationsSeries.add(extrapolations);
  }

  return extrapolationsSeries;
}

int solve(List<String> input) {
  List<List<int>> histories = parseInput(input);
  int res = 0;

  for (List<int> history in histories) {
    List<List<int>> extrapolationsSeries = calculateExtrapolations(history);

    int futurePrediction = 0;
    for (int i = extrapolationsSeries.length - 1; i > -1; i--) {
      futurePrediction = extrapolationsSeries[i][extrapolationsSeries[i].length - 1] + futurePrediction;
    }

    res += futurePrediction;
  }

  return res;
}

List<String> readFile(String fileName) {
  File file = File(fileName);
  List<String> lines = file.readAsLinesSync();
  return lines;
}

void main() {
  List<String> input = readFile("input.txt");
  print(solve(input));
}
