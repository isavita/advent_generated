
using System;
using System.IO;
using System.Linq;

class Item
{
    public int Cost { get; set; }
    public int Damage { get; set; }
    public int Armor { get; set; }
}

class Character
{
    public int HitPoints { get; set; }
    public int Damage { get; set; }
    public int Armor { get; set; }
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Character boss = new Character
        {
            HitPoints = ParseStat(lines[0]),
            Damage = ParseStat(lines[1]),
            Armor = ParseStat(lines[2])
        };

        Item[] weapons = {
            new Item { Cost = 8, Damage = 4 },
            new Item { Cost = 10, Damage = 5 },
            new Item { Cost = 25, Damage = 6 },
            new Item { Cost = 40, Damage = 7 },
            new Item { Cost = 74, Damage = 8 }
        };

        Item[] armors = {
            new Item { Cost = 0, Armor = 0 },
            new Item { Cost = 13, Armor = 1 },
            new Item { Cost = 31, Armor = 2 },
            new Item { Cost = 53, Armor = 3 },
            new Item { Cost = 75, Armor = 4 },
            new Item { Cost = 102, Armor = 5 }
        };

        Item[] rings = {
            new Item { Cost = 0 },
            new Item { Cost = 25, Damage = 1 },
            new Item { Cost = 50, Damage = 2 },
            new Item { Cost = 100, Damage = 3 },
            new Item { Cost = 20, Armor = 1 },
            new Item { Cost = 40, Armor = 2 },
            new Item { Cost = 80, Armor = 3 }
        };

        int maxCost = 0;
        foreach (var w in weapons)
        {
            foreach (var a in armors)
            {
                for (int ri = 0; ri < rings.Length; ri++)
                {
                    for (int rj = ri + 1; rj < rings.Length; rj++)
                    {
                        Character player = new Character
                        {
                            HitPoints = 100,
                            Damage = w.Damage + rings[ri].Damage + rings[rj].Damage,
                            Armor = a.Armor + rings[ri].Armor + rings[rj].Armor
                        };
                        int cost = w.Cost + a.Cost + rings[ri].Cost + rings[rj].Cost;
                        if (!PlayerWins(player, boss) && cost > maxCost)
                        {
                            maxCost = cost;
                        }
                    }
                }
            }
        }

        Console.WriteLine(maxCost);
    }

    static int ParseStat(string line)
    {
        return int.Parse(line.Split(": ")[1]);
    }

    static bool PlayerWins(Character player, Character boss)
    {
        int playerDamage = Math.Max(1, player.Damage - boss.Armor);
        int bossDamage = Math.Max(1, boss.Damage - player.Armor);

        int playerTurns = (boss.HitPoints + playerDamage - 1) / playerDamage;
        int bossTurns = (player.HitPoints + bossDamage - 1) / bossDamage;

        return playerTurns <= bossTurns;
    }
}
