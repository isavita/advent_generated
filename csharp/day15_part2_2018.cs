using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class BeverageBandits
{
    const int ElfStartHp = 200;
    const int GoblinAttackPower = 3;
    static readonly int[][] Directions = new int[][] { new[] { -1, 0 }, new[] { 0, -1 }, new[] { 0, 1 }, new[] { 1, 0 } };

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var grid = ParseGrid(lines);

        int part1 = SimulateCombat(CloneGrid(grid), 3, false);
        Console.WriteLine($"Part 1 Outcome: {part1}");

        int elfPower = 4;
        int part2;
        while (true)
        {
            part2 = SimulateCombat(CloneGrid(grid), elfPower, true);
            if (part2 != -1) break;
            elfPower++;
        }
        Console.WriteLine($"Part 2 Outcome: {part2} (Elf Attack Power: {elfPower})");
    }

    static char[][] ParseGrid(string[] lines)
    {
        var grid = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++) grid[i] = lines[i].ToCharArray();
        return grid;
    }

    static char[][] CloneGrid(char[][] src)
    {
        var dst = new char[src.Length][];
        for (int i = 0; i < src.Length; i++) dst[i] = (char[])src[i].Clone();
        return dst;
    }

    static int SimulateCombat(char[][] grid, int elfAttackPower, bool noElfDeaths)
    {
        int rows = grid.Length, cols = grid[0].Length;
        int fullRounds = 0;

        var hitPoints = new Dictionary<(int, int), int>();
        for (int r = 0; r < rows; r++)
            for (int c = 0; c < cols; c++)
                if (grid[r][c] == 'G' || grid[r][c] == 'E')
                    hitPoints[(r, c)] = ElfStartHp;

        while (true)
        {
            var units = GetUnits(grid);
            bool combatEnded = false;

            foreach (var unit in units)
            {
                if (!hitPoints.ContainsKey((unit.Row, unit.Col))) continue;

                if (!HasEnemies(grid, unit)) { combatEnded = true; break; }

                if (!IsInRange(grid, unit))
                {
                    var move = ChooseMove(grid, unit, hitPoints);
                    if (move != null)
                    {
                        char type = grid[unit.Row][unit.Col];
                        grid[unit.Row][unit.Col] = '.';
                        grid[move.Row][move.Col] = type;
                        int hp = hitPoints[(unit.Row, unit.Col)];
                        hitPoints.Remove((unit.Row, unit.Col));
                        hitPoints[(move.Row, move.Col)] = hp;
                        unit.Row = move.Row;
                        unit.Col = move.Col;
                    }
                }

                var target = ChooseAttackTarget(grid, unit, hitPoints);
                if (target != null)
                {
                    int power = grid[unit.Row][unit.Col] == 'E' ? elfAttackPower : GoblinAttackPower;
                    hitPoints[(target.Row, target.Col)] -= power;
                    if (hitPoints[(target.Row, target.Col)] <= 0)
                    {
                        if (noElfDeaths && grid[target.Row][target.Col] == 'E') return -1;
                        grid[target.Row][target.Col] = '.';
                        hitPoints.Remove((target.Row, target.Col));
                    }
                }
            }

            if (combatEnded) break;
            fullRounds++;
        }

        int totalHp = hitPoints.Values.Sum();
        return fullRounds * totalHp;
    }

    static List<Unit> GetUnits(char[][] grid)
    {
        var list = new List<Unit>();
        for (int r = 0; r < grid.Length; r++)
            for (int c = 0; c < grid[0].Length; c++)
                if (grid[r][c] == 'G' || grid[r][c] == 'E')
                    list.Add(new Unit(r, c));
        return list;
    }

    static bool HasEnemies(char[][] grid, Unit u)
    {
        char enemy = grid[u.Row][u.Col] == 'G' ? 'E' : 'G';
        for (int r = 0; r < grid.Length; r++)
            for (int c = 0; c < grid[0].Length; c++)
                if (grid[r][c] == enemy) return true;
        return false;
    }

    static bool IsInRange(char[][] grid, Unit u)
    {
        char enemy = grid[u.Row][u.Col] == 'G' ? 'E' : 'G';
        foreach (var d in Directions)
        {
            int nr = u.Row + d[0], nc = u.Col + d[1];
            if (nr >= 0 && nr < grid.Length && nc >= 0 && nc < grid[0].Length && grid[nr][nc] == enemy)
                return true;
        }
        return false;
    }

    static Unit ChooseMove(char[][] grid, Unit unit, Dictionary<(int, int), int> hp)
    {
        char enemy = grid[unit.Row][unit.Col] == 'G' ? 'E' : 'G';
        var inRange = new HashSet<(int, int)>();

        for (int r = 0; r < grid.Length; r++)
            for (int c = 0; c < grid[0].Length; c++)
                if (grid[r][c] == enemy)
                    foreach (var d in Directions)
                    {
                        int nr = r + d[0], nc = c + d[1];
                        if (nr >= 0 && nr < grid.Length && nc >= 0 && nc < grid[0].Length && grid[nr][nc] == '.')
                            inRange.Add((nr, nc));
                    }

        if (inRange.Count == 0) return null;

        var dist = new Dictionary<(int, int), int>();
        var prev = new Dictionary<(int, int), (int, int)>();
        var q = new Queue<(int, int)>();
        var start = (unit.Row, unit.Col);
        q.Enqueue(start);
        dist[start] = 0;

        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            foreach (var d in Directions)
            {
                int nr = cur.Item1 + d[0], nc = cur.Item2 + d[1];
                var nxt = (nr, nc);
                if (nr < 0 || nr >= grid.Length || nc < 0 || nc >= grid[0].Length) continue;
                if (grid[nr][nc] != '.' && !(nr == unit.Row && nc == unit.Col)) continue;
                if (dist.ContainsKey(nxt)) continue;
                dist[nxt] = dist[cur] + 1;
                prev[nxt] = cur;
                q.Enqueue(nxt);
            }
        }

        var reachable = inRange.Where(p => dist.ContainsKey(p)).ToList();
        if (!reachable.Any()) return null;

        int minDist = reachable.Min(p => dist[p]);
        var nearest = reachable.Where(p => dist[p] == minDist).OrderBy(p => p.Item1).ThenBy(p => p.Item2).First();

        var step = nearest;
        while (!prev[step].Equals(start)) step = prev[step];
        return new Unit(step.Item1, step.Item2);
    }

    static Unit ChooseAttackTarget(char[][] grid, Unit unit, Dictionary<(int, int), int> hp)
    {
        char enemy = grid[unit.Row][unit.Col] == 'G' ? 'E' : 'G';
        var candidates = new List<(int r, int c)>();
        foreach (var d in Directions)
        {
            int nr = unit.Row + d[0], nc = unit.Col + d[1];
            if (nr >= 0 && nr < grid.Length && nc >= 0 && nc < grid[0].Length && grid[nr][nc] == enemy)
                candidates.Add((nr, nc));
        }
        if (!candidates.Any()) return null;

        var target = candidates.OrderBy(p => hp[(p.r, p.c)])
                               .ThenBy(p => p.r)
                               .ThenBy(p => p.c)
                               .First();
        return new Unit(target.r, target.c);
    }

    class Unit
    {
        public int Row;
        public int Col;
        public Unit(int r, int c) { Row = r; Col = c; }
    }
}