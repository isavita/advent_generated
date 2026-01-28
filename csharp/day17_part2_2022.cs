using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int CHAMBER_WIDTH = 7;
    const int PROFILE_DEPTH = 30;
    static readonly int[][][] rockShapes = {
        new[] { new[] {0,0}, new[] {1,0}, new[] {2,0}, new[] {3,0} },
        new[] { new[] {1,0}, new[] {0,1}, new[] {1,1}, new[] {2,1}, new[] {1,2} },
        new[] { new[] {0,0}, new[] {1,0}, new[] {2,0}, new[] {2,1}, new[] {2,2} },
        new[] { new[] {0,0}, new[] {0,1}, new[] {0,2}, new[] {0,3} },
        new[] { new[] {0,0}, new[] {1,0}, new[] {0,1}, new[] {1,1} }
    };

    static List<(int x, long y)> TryMove(List<(int x, long y)> rock, char dir, HashSet<(int x, long y)> chamber)
    {
        var moved = new List<(int, long)>(rock.Count);
        foreach (var p in rock)
        {
            int nx = p.x;
            long ny = p.y;
            if (dir == '<') nx--;
            else if (dir == '>') nx++;
            else ny--;
            if (nx < 0 || nx >= CHAMBER_WIDTH || ny < 1) return null;
            if (chamber.Contains((nx, ny))) return null;
            moved.Add((nx, ny));
        }
        return moved;
    }

    static int[] GetProfile(HashSet<(int x, long y)> chamber, long highestY)
    {
        var prof = new int[CHAMBER_WIDTH];
        for (int x = 0; x < CHAMBER_WIDTH; x++)
        {
            bool found = false;
            long limit = Math.Max(1, highestY - PROFILE_DEPTH);
            for (long y = highestY; y >= limit; y--)
            {
                if (chamber.Contains((x, y)))
                {
                    prof[x] = (int)(highestY - y);
                    found = true;
                    break;
                }
            }
            if (!found) prof[x] = PROFILE_DEPTH + 1;
        }
        return prof;
    }

    static long Solve(string jetPattern, long totalRocks)
    {
        var chamber = new HashSet<(int x, long y)>();
        for (int x = 0; x < CHAMBER_WIDTH; x++) chamber.Add((x, 0));

        long highestY = 0;
        int jetLen = jetPattern.Length;
        long jetIdx = 0;
        int rockIdx = 0;
        var seen = new Dictionary<string, (long rockNum, long height)>();
        long addHeight = 0;
        long rockNum = 0;

        while (rockNum < totalRocks)
        {
            var shape = rockShapes[rockIdx % rockShapes.Length];
            int rockX = 2;
            long rockY = highestY + 4;
            var cur = new List<(int x, long y)>(shape.Length);
            foreach (var p in shape) cur.Add((rockX + p[0], rockY + p[1]));

            while (true)
            {
                char dir = jetPattern[(int)(jetIdx % jetLen)];
                jetIdx++;
                var horiz = dir == '>' ? TryMove(cur, '>', chamber) : (dir == '<' ? TryMove(cur, '<', chamber) : null);
                if (horiz != null) cur = horiz;
                var down = TryMove(cur, 'v', chamber);
                if (down != null) cur = down;
                else
                {
                    foreach (var pos in cur)
                    {
                        chamber.Add(pos);
                        if (pos.y > highestY) highestY = pos.y;
                    }
                    break;
                }
            }

            var profile = GetProfile(chamber, highestY);
            var key = $"{rockIdx % rockShapes.Length}|{jetIdx % jetLen}|{string.Join(',', profile)}";

            if (seen.TryGetValue(key, out var prev))
            {
                long cycleLen = rockNum - prev.rockNum;
                long cycleHeight = highestY - prev.height;
                long remaining = totalRocks - rockNum;
                if (remaining > 0)
                {
                    long cycles = remaining / cycleLen;
                    addHeight += cycles * cycleHeight;
                    rockNum += cycles * cycleLen;
                }
            }
            else
            {
                seen[key] = (rockNum, highestY);
            }

            rockNum++;
            rockIdx++;
        }

        return highestY + addHeight;
    }

    static void Main()
    {
        var jetPattern = File.ReadAllText("input.txt").Trim();
        long totalRocks = 1000000000000L;
        Console.WriteLine(Solve(jetPattern, totalRocks));
    }
}