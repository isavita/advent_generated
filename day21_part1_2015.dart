
import 'dart:io';

class Item {
  int cost;
  int damage;
  int armor;

  Item(this.cost, {this.damage = 0, this.armor = 0});
}

class Character {
  int hitPoints;
  int damage;
  int armor;

  Character(this.hitPoints, this.damage, this.armor);
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  Character boss = Character(parseStat(lines[0]), parseStat(lines[1]), parseStat(lines[2]));

  List<Item> weapons = [
    Item(8, damage: 4),
    Item(10, damage: 5),
    Item(25, damage: 6),
    Item(40, damage: 7),
    Item(74, damage: 8),
  ];

  List<Item> armors = [
    Item(0),
    Item(13, armor: 1),
    Item(31, armor: 2),
    Item(53, armor: 3),
    Item(75, armor: 4),
    Item(102, armor: 5),
  ];

  List<Item> rings = [
    Item(0),
    Item(25, damage: 1),
    Item(50, damage: 2),
    Item(100, damage: 3),
    Item(20, armor: 1),
    Item(40, armor: 2),
    Item(80, armor: 3),
  ];

  int minCost = 2147483647; // Set to maximum int value.
  for (Item w in weapons) {
    for (Item a in armors) {
      for (int ri = 0; ri < rings.length; ri++) {
        for (int rj = ri + 1; rj < rings.length; rj++) {
          Character player = Character(100, w.damage, a.armor);
          player.damage += rings[ri].damage + rings[rj].damage;
          player.armor += rings[ri].armor + rings[rj].armor;
          int cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost;
          if (playerWins(player, boss) && cost < minCost) {
            minCost = cost;
          }
        }
      }
    }
  }

  print(minCost);
}

int parseStat(String line) {
  List<String> parts = line.split(': ');
  return int.parse(parts[1]);
}

bool playerWins(Character player, Character boss) {
  int playerDamage = max(1, player.damage - boss.armor);
  int bossDamage = max(1, boss.damage - player.armor);

  int playerTurns = (boss.hitPoints + playerDamage - 1) ~/ playerDamage;
  int bossTurns = (player.hitPoints + bossDamage - 1) ~/ bossDamage;

  return playerTurns <= bossTurns;
}

int max(int a, int b) {
  return a > b ? a : b;
}
