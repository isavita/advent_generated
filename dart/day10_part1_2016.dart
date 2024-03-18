import 'dart:io';

class Bot {
  late String lowTo;
  late String highTo;
  List<int> chips = [];
}

void main() {
  var file = File('input.txt');
  var bots = <String, Bot>{};
  var valueRegex = RegExp(r'value (\d+) goes to (bot \d+)');
  var givesRegex = RegExp(r'(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)');

  file.readAsLinesSync().forEach((line) {
    if (valueRegex.hasMatch(line)) {
      var matches = valueRegex.firstMatch(line);
      var value = int.parse(matches![1]!);
      var botID = matches[2]!;
      
      bots.putIfAbsent(botID, () => Bot());
      bots[botID]!.chips.add(value);

    } else if (givesRegex.hasMatch(line)) {
      var matches = givesRegex.firstMatch(line);
      var botID = matches![1]!;
      var lowTo = matches[2]!;
      var highTo = matches[3]!;
      
      bots.putIfAbsent(botID, () => Bot());
      bots[botID]!.lowTo = lowTo;
      bots[botID]!.highTo = highTo;
    }
  });

  while (true) {
    var action = false;
    bots.forEach((botID, b) {
      if (b.chips.length == 2) {
        action = true;
        var low = b.chips.reduce((a, b) => a < b ? a : b);
        var high = b.chips.reduce((a, b) => a > b ? a : b);
        if (low == 17 && high == 61) {
          print(botID);
          exit(0);
        }
        b.chips.clear();

        giveChip(bots, b.lowTo, low);
        giveChip(bots, b.highTo, high);
      }
    });
    if (!action) {
      break;
    }
  }
}

void giveChip(Map<String, Bot> bots, String target, int value) {
  bots.putIfAbsent(target, () => Bot());
  bots[target]!.chips.add(value);
}