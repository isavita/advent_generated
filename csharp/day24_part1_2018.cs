
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public class Group
{
    public int Id { get; set; }
    public int ArmyId { get; set; }
    public int Units { get; set; }
    public int HitPoints { get; set; }
    public int AttackDamage { get; set; }
    public string AttackType { get; set; }
    public int Initiative { get; set; }
    public List<string> Immunities { get; set; }
    public List<string> Weaknesses { get; set; }
    public Group Target { get; set; }
    public Group Attacker { get; set; }

    public int EffectivePower => Units * AttackDamage;
}

public class Program
{
    private static List<Group> ParseInput(string filename)
    {
        var groups = new List<Group>();
        int groupId = 0;
        int armyId = 0;

        foreach (var line in File.ReadLines(filename))
        {
            if (line.StartsWith("Immune System:"))
            {
                armyId = 1;
            }
            else if (line.StartsWith("Infection:"))
            {
                armyId = 2;
            }
            else if (!string.IsNullOrEmpty(line))
            {
                var match = Regex.Match(line, @"(\d+) units each with (\d+) hit points (?:\(([^\)]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)");
                if (match.Success)
                {
                    var group = new Group
                    {
                        Id = ++groupId,
                        ArmyId = armyId,
                        Units = int.Parse(match.Groups[1].Value),
                        HitPoints = int.Parse(match.Groups[2].Value),
                        AttackDamage = int.Parse(match.Groups[4].Value),
                        AttackType = match.Groups[5].Value,
                        Initiative = int.Parse(match.Groups[6].Value),
                        Immunities = new List<string>(),
                        Weaknesses = new List<string>()
                    };

                    var modifiers = match.Groups[3].Value;
                    if (!string.IsNullOrEmpty(modifiers))
                    {
                        foreach (var modifier in modifiers.Split(';'))
                        {
                            var trimmedModifier = modifier.Trim();
                            if (trimmedModifier.StartsWith("immune to"))
                            {
                                group.Immunities.AddRange(trimmedModifier.Substring(9).Trim().Split(',').Select(x => x.Trim()));
                            }
                            else if (trimmedModifier.StartsWith("weak to"))
                            {
                                group.Weaknesses.AddRange(trimmedModifier.Substring(7).Trim().Split(',').Select(x => x.Trim()));
                            }
                        }
                    }

                    groups.Add(group);
                }
            }
        }

        return groups;
    }

    private static int CalculateDamage(Group attacker, Group defender)
    {
        if (attacker.Units <= 0 || defender.Units <= 0)
        {
            return 0;
        }

        if (defender.Immunities.Contains(attacker.AttackType))
        {
            return 0;
        }

        var damage = attacker.EffectivePower;
        if (defender.Weaknesses.Contains(attacker.AttackType))
        {
            damage *= 2;
        }

        return damage;
    }

    private static void Main()
    {
        var groups = ParseInput("input.txt");

        while (true)
        {
            var immuneAlive = groups.Where(g => g.ArmyId == 1 && g.Units > 0).Sum(g => g.Units);
            var infectionAlive = groups.Where(g => g.ArmyId == 2 && g.Units > 0).Sum(g => g.Units);

            if (immuneAlive == 0 || infectionAlive == 0)
            {
                Console.WriteLine(immuneAlive + infectionAlive);
                break;
            }

            foreach (var group in groups)
            {
                group.Target = null;
                group.Attacker = null;
            }

            var activeGroups = groups.Where(g => g.Units > 0).OrderByDescending(g => g.EffectivePower).ThenByDescending(g => g.Initiative).ToList();

            foreach (var attacker in activeGroups)
            {
                var bestTarget = activeGroups.Where(g => g.ArmyId != attacker.ArmyId && g.Attacker == null)
                                             .OrderByDescending(g => CalculateDamage(attacker, g))
                                             .ThenByDescending(g => g.EffectivePower)
                                             .ThenByDescending(g => g.Initiative)
                                             .FirstOrDefault();

                if (bestTarget != null)
                {
                    attacker.Target = bestTarget;
                    bestTarget.Attacker = attacker;
                }
            }

            var initiativeOrder = activeGroups.OrderByDescending(g => g.Initiative).ToList();
            var totalKills = 0;

            foreach (var attacker in initiativeOrder)
            {
                if (attacker.Units > 0 && attacker.Target != null && attacker.Target.Units > 0)
                {
                    var damage = CalculateDamage(attacker, attacker.Target);
                    var unitsKilled = Math.Min(attacker.Target.Units, damage / attacker.Target.HitPoints);
                    attacker.Target.Units -= unitsKilled;
                    totalKills += unitsKilled;
                }
            }

            if (totalKills == 0)
            {
                Console.WriteLine(0);
                break;
            }
        }
    }
}
