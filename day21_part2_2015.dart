
import 'dart:io';

class Item {
  int cost;
  int damage;
  int armor;

  Item({required this.cost, this.damage = 0, this.armor = 0});
}

class Character {
  int hitPoints;
  int damage;
  int armor;

  Character({required this.hitPoints, required this.damage, required this.armor});
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  Character boss = Character(
    hitPoints: parseStat(lines[0]),
    damage: parseStat(lines[1]),
    armor: parseStat(lines[2]),
  );

  List<Item> weapons = [
    Item(cost: 8, damage: 4),
    Item(cost: 10, damage: 5),
    Item(cost: 25, damage: 6),
    Item(cost: 40, damage: 7),
    Item(cost: 74, damage: 8),
  ];

  List<Item> armors = [
    Item(cost: 0, armor: 0),
    Item(cost: 13, armor: 1),
    Item(cost: 31, armor: 2),
    Item(cost: 53, armor: 3),
    Item(cost: 75, armor: 4),
    Item(cost: 102, armor: 5),
  ];

  List<Item> rings = [
    Item(cost: 0),
    Item(cost: 25, damage: 1),
    Item(cost: 50, damage: 2),
    Item(cost: 100, damage: 3),
    Item(cost: 20, armor: 1),
    Item(cost: 40, armor: 2),
    Item(cost: 80, armor: 3),
  ];

  int maxCost = 0;
  for (Item w in weapons) {
    for (Item a in armors) {
      for (int ri = 0; ri < rings.length; ri++) {
        for (int rj = ri + 1; rj < rings.length; rj++) {
          Character player = Character(
            hitPoints: 100,
            damage: w.damage + rings[ri].damage + rings[rj].damage,
            armor: a.armor + rings[ri].armor + rings[rj].armor,
          );
          int cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost;
          if (!playerWins(player, boss) && cost > maxCost) {
            maxCost = cost;
          }
        }
      }
    }
  }

  print(maxCost);
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
