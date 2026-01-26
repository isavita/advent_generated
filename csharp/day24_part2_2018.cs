
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

class Group
{
    public int Units, Hp, AttackDamage, Initiative;
    public string AttackType;
    public bool IsImmune;
    public HashSet<string> Weaknesses, Immunities;
    public Group(int u, int h, HashSet<string> wk, HashSet<string> im, int ad, string at, int inits, bool imSys)
    {
        Units = u; Hp = h; Weaknesses = wk; Immunities = im; AttackDamage = ad; AttackType = at; Initiative = inits; IsImmune = imSys;
    }
    public Group(Group o) : this(o.Units, o.Hp, new HashSet<string>(o.Weaknesses), new HashSet<string>(o.Immunities),
        o.AttackDamage, o.AttackType, o.Initiative, o.IsImmune) { }
    public int EffectivePower => Units * AttackDamage;
}

class Program
{
    static readonly Regex GroupPat = new Regex(@"(\d+) units each with (\d+) hit points (?:\(([^)]*)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)", RegexOptions.Compiled);
    static void Main()
    {
        var immune = new List<Group>();
        var infection = new List<Group>();
        ReadInput("input.txt", immune, infection);
        int part1 = SimulateCombat(Clone(immune), Clone(infection), 0);
        Console.WriteLine($"Part One - Winning army units: {part1}");
        int boost = 0, part2 = 0;
        while (true)
        {
            boost++;
            var im = Clone(immune);
            var inf = Clone(infection);
            int res = SimulateCombat(im, inf, boost);
            if (res > 0 && im.Sum(g => g.Units) > 0 && inf.Sum(g => g.Units) == 0)
            {
                part2 = im.Sum(g => g.Units);
                break;
            }
            if (boost > 1000) break;
        }
        Console.WriteLine($"Part Two - Immune system units with boost: {part2}");
    }
    static void ReadInput(string file, List<Group> immune, List<Group> infection)
    {
        var lines = File.ReadAllLines(file);
        List<Group> cur = null;
        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line == "") continue;
            if (line == "Immune System:") cur = immune;
            else if (line == "Infection:") cur = infection;
            else
            {
                var m = GroupPat.Match(line);
                int u = int.Parse(m.Groups[1].Value);
                int hp = int.Parse(m.Groups[2].Value);
                string wi = m.Groups[3].Value;
                int dmg = int.Parse(m.Groups[4].Value);
                string at = m.Groups[5].Value;
                int init = int.Parse(m.Groups[6].Value);
                var wk = new HashSet<string>();
                var im = new HashSet<string>();
                if (!string.IsNullOrEmpty(wi))
                {
                    foreach (var part in wi.Split("; "))
                        if (part.StartsWith("weak to "))
                            wk.UnionWith(part.Substring(8).Split(", "));
                        else if (part.StartsWith("immune to "))
                            im.UnionWith(part.Substring(10).Split(", "));
                }
                cur.Add(new Group(u, hp, wk, im, dmg, at, init, cur == immune));
            }
        }
    }
    static int SimulateCombat(List<Group> immune, List<Group> infection, int boost)
    {
        foreach (var g in immune) g.AttackDamage += boost;
        while (immune.Any() && infection.Any())
        {
            var all = immune.Concat(infection).OrderByDescending(g => g.EffectivePower).ThenByDescending(g => g.Initiative).ToList();
            var targets = new Dictionary<Group, Group>();
            var taken = new HashSet<Group>();
            foreach (var a in all)
            {
                var enemies = a.IsImmune ? infection : immune;
                var cand = enemies.Where(d => !taken.Contains(d))
                    .Select(d => new { d, dmg = Damage(a, d) })
                    .Where(x => x.dmg > 0)
                    .OrderByDescending(x => x.dmg)
                    .ThenByDescending(x => x.d.EffectivePower)
                    .ThenByDescending(x => x.d.Initiative)
                    .FirstOrDefault();
                if (cand != null) { targets[a] = cand.d; taken.Add(cand.d); }
            }
            var attackOrder = all.OrderByDescending(g => g.Initiative).ToList();
            bool any = false;
            foreach (var a in attackOrder)
            {
                if (a.Units <= 0 || !targets.TryGetValue(a, out var d)) continue;
                int dmg = Damage(a, d);
                int killed = Math.Min(d.Units, dmg / d.Hp);
                if (killed > 0) any = true;
                d.Units -= killed;
            }
            if (!any) return -1;
            immune.RemoveAll(g => g.Units <= 0);
            infection.RemoveAll(g => g.Units <= 0);
        }
        return immune.Any() ? immune.Sum(g => g.Units) : -infection.Sum(g => g.Units);
    }
    static int Damage(Group a, Group d)
    {
        if (d.Immunities.Contains(a.AttackType)) return 0;
        int dmg = a.EffectivePower;
        if (d.Weaknesses.Contains(a.AttackType)) dmg *= 2;
        return dmg;
    }
    static List<Group> Clone(List<Group> src) => src.Select(g => new Group(g)).ToList();
}
