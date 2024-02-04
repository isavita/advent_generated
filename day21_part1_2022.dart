import 'dart:io';

void main() {
  var file = File('input.txt');
  var jobs = <String, String>{};
  var results = <String, int>{};

  file.readAsLinesSync().forEach((line) {
    var parts = line.split(': ');
    jobs[parts[0]] = parts[1];
  });

  print(calculate('root', jobs, results));
}

int calculate(String monkey, Map<String, String> jobs, Map<String, int> results) {
  if (results.containsKey(monkey)) {
    return results[monkey]!;
  }

  var job = jobs[monkey];
  if (job == null) {
    throw Exception('Monkey not found: $monkey');
  }

  if (int.tryParse(job) != null) {
    results[monkey] = int.parse(job);
    return int.parse(job);
  }

  var parts = job.split(' ');
  var a = calculate(parts[0], jobs, results);
  var b = calculate(parts[2], jobs, results);

  late int result;
  switch (parts[1]) {
    case '+':
      result = a + b;
      break;
    case '-':
      result = a - b;
      break;
    case '*':
      result = a * b;
      break;
    case '/':
      result = a ~/ b;
      break;
    default:
      throw Exception('Unknown operation: ${parts[1]}');
  }

  results[monkey] = result;
  return result;
}