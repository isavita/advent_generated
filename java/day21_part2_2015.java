
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class RPGSimulator {

    static class Item {
        int cost;
        int damage;
        int armor;

        public Item(int cost, int damage, int armor) {
            this.cost = cost;
            this.damage = damage;
            this.armor = armor;
        }
    }

    static class Character {
        int hp;
        int damage;
        int armor;

        public Character(int hp, int damage, int armor) {
            this.hp = hp;
            this.damage = damage;
            this.armor = armor;
        }
    }

    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            int bossHp = 0;
            int bossDamage = 0;
            int bossArmor = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                if (line.startsWith("Hit Points:")) {
                    bossHp = Integer.parseInt(line.split(": ")[1]);
                } else if (line.startsWith("Damage:")) {
                    bossDamage = Integer.parseInt(line.split(": ")[1]);
                } else if (line.startsWith("Armor:")) {
                    bossArmor = Integer.parseInt(line.split(": ")[1]);
                }
            }
            scanner.close();

            List<Item> weapons = List.of(
                    new Item(8, 4, 0),
                    new Item(10, 5, 0),
                    new Item(25, 6, 0),
                    new Item(40, 7, 0),
                    new Item(74, 8, 0)
            );

            List<Item> armors = List.of(
                    new Item(0, 0, 0), // No armor
                    new Item(13, 0, 1),
                    new Item(31, 0, 2),
                    new Item(53, 0, 3),
                    new Item(75, 0, 4),
                    new Item(102, 0, 5)
            );

            List<Item> rings = List.of(
                    new Item(0, 0, 0), // No ring
                    new Item(25, 1, 0),
                    new Item(50, 2, 0),
                    new Item(100, 3, 0),
                    new Item(20, 0, 1),
                    new Item(40, 0, 2),
                    new Item(80, 0, 3)
            );

            int minCostToWin = Integer.MAX_VALUE;
            int maxCostToLose = 0;

            for (Item weapon : weapons) {
                for (Item armor : armors) {
                    for (int i = 0; i < rings.size(); i++) {
                        for (int j = 0; j < rings.size(); j++) {
                            if (i != j || (i == 0 && j == 0)) {
                                Item ring1 = rings.get(i);
                                Item ring2 = rings.get(j);

                                int totalCost = weapon.cost + armor.cost + ring1.cost + ring2.cost;
                                int totalDamage = weapon.damage + ring1.damage + ring2.damage;
                                int totalArmor = armor.armor + ring1.armor + ring2.armor;

                                Character player = new Character(100, totalDamage, totalArmor);
                                Character boss = new Character(bossHp, bossDamage, bossArmor);

                                if (simulateFight(player, boss)) {
                                    minCostToWin = Math.min(minCostToWin, totalCost);
                                } else {
                                    maxCostToLose = Math.max(maxCostToLose, totalCost);
                                }
                            }
                        }
                    }
                }
            }
            System.out.println(minCostToWin);
            System.out.println(maxCostToLose);

        } catch (FileNotFoundException e) {
            System.out.println("File not found: " + e.getMessage());
        }
    }

    static boolean simulateFight(Character player, Character boss) {
        while (player.hp > 0 && boss.hp > 0) {
            int playerDamage = Math.max(1, player.damage - boss.armor);
            boss.hp -= playerDamage;
            if (boss.hp <= 0) {
                return true;
            }
            int bossDamage = Math.max(1, boss.damage - player.armor);
            player.hp -= bossDamage;
        }
        return player.hp > 0;
    }
}
