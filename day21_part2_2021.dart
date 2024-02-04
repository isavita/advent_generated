import 'dart:io';

void main() {
  File file = File('input.txt');
  String input = file.readAsStringSync().trim();
  List<int> positions = parseInput(input);
  List<int> scores = [0, 0];
  List<int> result = play([positions[0], positions[1]], scores, 3, true, {});

  if (result[0] > result[1]) {
    print(result[0]);
  } else {
    print(result[1]);
  }
}

List<int> play(List<int> positions, List<int> scores, int rollsLeftInTurn, bool isPlayer1sTurn, Map<String, List<int>> memo) {
  String key = '$positions$scores$rollsLeftInTurn$isPlayer1sTurn';
  if (memo.containsKey(key)) {
    return memo[key]!;
  }

  int playerIndex = isPlayer1sTurn ? 0 : 1;
  List<int> scoresCopy = List.from(scores);
  if (rollsLeftInTurn == 0) {
    scoresCopy[playerIndex] += positions[playerIndex];

    if (scoresCopy[playerIndex] >= 21) {
      if (playerIndex == 0) {
        return [1, 0];
      }
      return [0, 1];
    }

    isPlayer1sTurn = !isPlayer1sTurn;
    rollsLeftInTurn = 3;

    playerIndex = (playerIndex + 1) % 2;
  }

  int wins1 = 0;
  int wins2 = 0;
  for (int roll = 1; roll <= 3; roll++) {
    List<int> positionsCopy = List.from(positions);
    positionsCopy[playerIndex] += roll;
    if (positionsCopy[playerIndex] > 10) {
      positionsCopy[playerIndex] -= 10;
    }
    List<int> result = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo);
    wins1 += result[0];
    wins2 += result[1];
  }

  memo[key] = [wins1, wins2];
  return [wins1, wins2];
}

List<int> parseInput(String input) {
  List<int> ans = [];
  List<String> lines = input.split('\n');
  for (String line in lines) {
    List<String> parts = line.split(' ');
    int player = int.parse(parts[1]);
    int startingPosition = int.parse(parts[4]);
    ans.add(startingPosition);
  }
  return ans;
}