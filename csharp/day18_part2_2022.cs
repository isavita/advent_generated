
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    struct Point3D
    {
        public int X, Y, Z;
        public Point3D(int x, int y, int z) { X = x; Y = y; Z = z; }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var cubes = new HashSet<(int, int, int)>();
        int minX = int.MaxValue, minY = int.MaxValue, minZ = int.MaxValue;
        int maxX = int.MinValue, maxY = int.MinValue, maxZ = int.MinValue;

        foreach (var line in lines)
        {
            if (string.IsNullOrWhiteSpace(line)) continue;
            var parts = line.Split(',');
            int x = int.Parse(parts[0]);
            int y = int.Parse(parts[1]);
            int z = int.Parse(parts[2]);
            cubes.Add((x, y, z));
            minX = Math.Min(minX, x); maxX = Math.Max(maxX, x);
            minY = Math.Min(minY, y); maxY = Math.Max(maxY, y);
            minZ = Math.Min(minZ, z); maxZ = Math.Max(maxZ, z);
        }

        minX--; minY--; minZ--;
        maxX++; maxY++; maxZ++;

        var seen = new HashSet<(int, int, int)>();
        var q = new Queue<Point3D>();
        q.Enqueue(new Point3D(minX, minY, minZ));
        seen.Add((minX, minY, minZ));

        int faces = 0;
        var dirs = new (int dx, int dy, int dz)[]
        {
            (-1, 0, 0), (1, 0, 0),
            (0, -1, 0), (0, 1, 0),
            (0, 0, -1), (0, 0, 1)
        };

        while (q.Count > 0)
        {
            var curr = q.Dequeue();
            foreach (var d in dirs)
            {
                int nx = curr.X + d.dx;
                int ny = curr.Y + d.dy;
                int nz = curr.Z + d.dz;

                if (nx < minX || nx > maxX ||
                    ny < minY || ny > maxY ||
                    nz < minZ || nz > maxZ) continue;

                if (cubes.Contains((nx, ny, nz)))
                {
                    faces++;
                }
                else if (!seen.Contains((nx, ny, nz)))
                {
                    seen.Add((nx, ny, nz));
                    q.Enqueue(new Point3D(nx, ny, nz));
                }
            }
        }

        Console.WriteLine(faces);
    }
}
