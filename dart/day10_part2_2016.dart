import 'dart:io';
import 'dart:collection';
import 'dart:convert';
import 'dart:core';
import 'dart:math';

class Bot {
  List<int> chips = [];
  String lowTo = '';
  String highTo = '';
}

void main() async {
  Map<String, Bot> bots = {};
  Map<String, int> outputs = {};
  RegExp valueRegex = RegExp(r'value (\d+) goes to (bot \d+)');
  RegExp givesRegex = RegExp(r'(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)');

  final lines = await File('input.txt').readAsLines();
  for (final line in lines) {
    if (valueRegex.hasMatch(line)) {
      var matches = valueRegex.firstMatch(line)!;
      var value = int.parse(matches.group(1)!);
      var botId = matches.group(2)!;
      if (!bots.containsKey(botId)) {
        bots[botId] = Bot();
      }
      bots[botId]!.chips.add(value);
    } else if (givesRegex.hasMatch(line)) {
      var matches = givesRegex.firstMatch(line)!;
      var botId = matches.group(1)!;
      var lowTo = matches.group(2)!;
      var highTo = matches.group(3)!;
      if (!bots.containsKey(botId)) {
        bots[botId] = Bot();
      }
      bots[botId]!.lowTo = lowTo;
      bots[botId]!.highTo = highTo;
    }
  }

  while (true) {
    bool action = false;
    bots.forEach((_, b) {
      if (b.chips.length == 2) {
        action = true;
        var low = min(b.chips[0], b.chips[1]);
        var high = max(b.chips[0], b.chips[1]);
        b.chips.clear();
        giveChip(bots, outputs, b.lowTo, low);
        giveChip(bots, outputs, b.highTo, high);
      }
    });
    if (!action) {
      break;
    }
  }

  int result = outputs['output 0']! * outputs['output 1']! * outputs['output 2']!;
  print(result);
}

void giveChip(Map<String, Bot> bots, Map<String, int> outputs, String target, int value) {
  if (target.startsWith('bot')) {
    if (!bots.containsKey(target)) {
      bots[target] = Bot();
    }
    bots[target]!.chips.add(value);
  } else if (target.startsWith('output')) {
    outputs[target] = value;
  }
}