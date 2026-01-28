
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static int HP = 200;
    static int Power = 3;
    static (int dx, int dy)[] Directions = { (0, -1), (-1, 0), (1, 0), (0, 1) };

    class Unit
    {
        public int X, Y, Hp, Atk;
        public char Type;
        public bool Dead => Hp <= 0;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int h = lines.Length, w = lines[0].Length;
        var grid = new char[h, w];
        var units = new List<Unit>();

        for (int y = 0; y < h; y++)
        {
            for (int x = 0; x < w; x++)
            {
                grid[y, x] = lines[y][x];
                if (grid[y, x] == 'E' || grid[y, x] == 'G')
                {
                    units.Add(new Unit { X = x, Y = y, Type = grid[y, x], Hp = HP, Atk = Power });
                }
            }
        }

        int rounds = 0;
        while (true)
        {
            units = units.OrderBy(u => u.Y).ThenBy(u => u.X).ToList();
            bool fullRound = true;

            for (int i = 0; i < units.Count; i++)
            {
                var u = units[i];
                if (u.Dead) continue;

                var targets = units.Where(t => !t.Dead && t.Type != u.Type).ToList();
                if (targets.Count == 0)
                {
                    fullRound = false;
                    goto EndCombat;
                }

                if (!targets.Any(t => Math.Abs(t.X - u.X) + Math.Abs(t.Y - u.Y) == 1))
                {
                    Move(u, targets, grid, h, w);
                }

                Attack(u, targets, grid);
            }
            rounds++;
        }

    EndCombat:
        Console.WriteLine(rounds * units.Where(u => !u.Dead).Sum(u => u.Hp));
    }

    static void Move(Unit u, List<Unit> targets, char[,] grid, int h, int w)
    {
        var inRange = new HashSet<(int x, int y)>();
        foreach (var t in targets)
        {
            foreach (var d in Directions)
            {
                int nx = t.X + d.dx, ny = t.Y + d.dy;
                if (grid[ny, nx] == '.') inRange.Add((nx, ny));
            }
        }

        var dists = GetDistances(u.X, u.Y, grid, h, w);
        var reachable = inRange.Where(r => dists.ContainsKey(r))
                               .OrderBy(r => dists[r])
                               .ThenBy(r => r.y)
                               .ThenBy(r => r.x)
                               .ToList();

        if (reachable.Count > 0)
        {
            var chosen = reachable[0];
            var stepDists = GetDistances(chosen.x, chosen.y, grid, h, w);
            var bestStep = Directions.Select(d => (u.X + d.dx, u.Y + d.dy))
                                     .Where(p => grid[p.Item2, p.Item1] == '.' && stepDists.ContainsKey(p))
                                     .OrderBy(p => stepDists[p])
                                     .ThenBy(p => p.Item2)
                                     .ThenBy(p => p.Item1)
                                     .First();

            grid[u.Y, u.X] = '.';
            u.X = bestStep.Item1;
            u.Y = bestStep.Item2;
            grid[u.Y, u.X] = u.Type;
        }
    }

    static Dictionary<(int, int), int> GetDistances(int sx, int sy, char[,] grid, int h, int w)
    {
        var dists = new Dictionary<(int, int), int> { [(sx, sy)] = 0 };
        var q = new Queue<(int x, int y)>();
        q.Enqueue((sx, sy));
        while (q.Count > 0)
        {
            var (x, y) = q.Dequeue();
            foreach (var d in Directions)
            {
                int nx = x + d.dx, ny = y + d.dy;
                if (grid[ny, nx] == '.' && !dists.ContainsKey((nx, ny)))
                {
                    dists[(nx, ny)] = dists[(x, y)] + 1;
                    q.Enqueue((nx, ny));
                }
            }
        }
        return dists;
    }

    static void Attack(Unit u, List<Unit> targets, char[,] grid)
    {
        var adjacent = targets.Where(t => Math.Abs(t.X - u.X) + Math.Abs(t.Y - u.Y) == 1)
                              .OrderBy(t => t.Hp)
                              .ThenBy(t => t.Y)
                              .ThenBy(t => t.X)
                              .FirstOrDefault();

        if (adjacent != null)
        {
            adjacent.Hp -= u.Atk;
            if (adjacent.Dead) grid[adjacent.Y, adjacent.X] = '.';
        }
    }
}
