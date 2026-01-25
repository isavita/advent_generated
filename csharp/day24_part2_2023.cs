
using System;
using System.IO;
using System.Linq;

public class Hailstone
{
    public long Px { get; set; }
    public long Py { get; set; }
    public long Pz { get; set; }
    public long Vx { get; set; }
    public long Vy { get; set; }
    public long Vz { get; set; }
}

class Program
{
    static void Main(string[] args)
    {
        var hailstones = ParseInput("input.txt");
        SolvePart1(hailstones);
        SolvePart2(hailstones);
    }

    static Hailstone[] ParseInput(string filename)
    {
        var lines = File.ReadAllLines(filename);
        var hailstones = new Hailstone[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            var parts = lines[i].Split(new[] { '@', ',', ' ' }, StringSplitOptions.RemoveEmptyEntries);
            hailstones[i] = new Hailstone
            {
                Px = long.Parse(parts[0]),
                Py = long.Parse(parts[1]),
                Pz = long.Parse(parts[2]),
                Vx = long.Parse(parts[3]),
                Vy = long.Parse(parts[4]),
                Vz = long.Parse(parts[5])
            };
        }

        return hailstones;
    }

    static void SolvePart1(Hailstone[] hailstones)
    {
        const double minCoord = 200000000000000.0;
        const double maxCoord = 400000000000000.0;
        long intersectionCount = 0;

        for (int i = 0; i < hailstones.Length; i++)
        {
            for (int j = i + 1; j < hailstones.Length; j++)
            {
                var h1 = hailstones[i];
                var h2 = hailstones[j];

                var det = h1.Vx * h2.Vy - h1.Vy * h2.Vx;
                if (det == 0) continue;

                var t1 = ((double)(h2.Px - h1.Px) * h2.Vy - (h2.Py - h1.Py) * h2.Vx) / det;
                var t2 = ((double)(h2.Px - h1.Px) * h1.Vy - (h2.Py - h1.Py) * h1.Vx) / det;

                if (t1 > 0 && t2 > 0)
                {
                    var ix = h1.Px + h1.Vx * t1;
                    var iy = h1.Py + h1.Vy * t1;

                    if (ix >= minCoord && ix <= maxCoord && iy >= minCoord && iy <= maxCoord)
                    {
                        intersectionCount++;
                    }
                }
            }
        }

        Console.WriteLine($"Part 1: {intersectionCount}");
    }

    static void SolvePart2(Hailstone[] hailstones)
    {
        if (hailstones.Length < 3)
        {
            Console.WriteLine("Part 2: Not enough data points.");
            return;
        }

        var h0 = hailstones[0];
        var h1 = hailstones[1];
        var h2 = hailstones[2];

        var dvx1 = h0.Vx - h1.Vx;
        var dvy1 = h0.Vy - h1.Vy;
        var dvz1 = h0.Vz - h1.Vz;
        var dpx1 = h0.Px - h1.Px;
        var dpy1 = h0.Py - h1.Py;
        var dpz1 = h0.Pz - h1.Pz;

        var dvx2 = h0.Vx - h2.Vx;
        var dvy2 = h0.Vy - h2.Vy;
        var dvz2 = h0.Vz - h2.Vz;
        var dpx2 = h0.Px - h2.Px;
        var dpy2 = h0.Py - h2.Py;
        var dpz2 = h0.Pz - h2.Pz;

        var A = new double[6, 7];

        A[0, 1] = dvz1; A[0, 2] = -dvy1; A[0, 4] = dpz1; A[0, 5] = -dpy1;
        A[1, 0] = -dvz1; A[1, 2] = dvx1; A[1, 3] = -dpz1; A[1, 5] = dpx1;
        A[2, 0] = dvy1; A[2, 1] = -dvx1; A[2, 3] = dpy1; A[2, 4] = -dpx1;
        A[3, 1] = dvz2; A[3, 2] = -dvy2; A[3, 4] = dpz2; A[3, 5] = -dpy2;
        A[4, 0] = -dvz2; A[4, 2] = dvx2; A[4, 3] = -dpz2; A[4, 5] = dpx2;
        A[5, 0] = dvy2; A[5, 1] = -dvx2; A[5, 3] = dpy2; A[5, 4] = -dpx2;

        A[0, 6] = h0.Py * h0.Vz - h0.Pz * h0.Vy - (h1.Py * h1.Vz - h1.Pz * h1.Vy);
        A[1, 6] = h0.Pz * h0.Vx - h0.Px * h0.Vz - (h1.Pz * h1.Vx - h1.Px * h1.Vz);
        A[2, 6] = h0.Px * h0.Vy - h0.Py * h0.Vx - (h1.Px * h1.Vy - h1.Py * h1.Vx);
        A[3, 6] = h0.Py * h0.Vz - h0.Pz * h0.Vy - (h2.Py * h2.Vz - h2.Pz * h2.Vy);
        A[4, 6] = h0.Pz * h0.Vx - h0.Px * h0.Vz - (h2.Pz * h2.Vx - h2.Px * h2.Vz);
        A[5, 6] = h0.Px * h0.Vy - h0.Py * h0.Vx - (h2.Px * h2.Vy - h2.Py * h2.Vx);

        SolveLinearSystem(A);

        var prx = Math.Round(A[0, 6]);
        var pry = Math.Round(A[1, 6]);
        var prz = Math.Round(A[2, 6]);

        Console.WriteLine($"Part 2: {prx + pry + prz}");
    }

    static void SolveLinearSystem(double[,] A)
    {
        int n = (int)Math.Sqrt(A.Length);
        for (int i = 0; i < n; i++)
        {
            int maxRow = i;
            for (int k = i + 1; k < n; k++)
            {
                if (Math.Abs(A[k, i]) > Math.Abs(A[maxRow, i])) maxRow = k;
            }
            for (int k = i; k <= n; k++)
            {
                var temp = A[i, k];
                A[i, k] = A[maxRow, k];
                A[maxRow, k] = temp;
            }

            for (int k = i + 1; k < n; k++)
            {
                var factor = A[k, i] / A[i, i];
                for (int j = i; j <= n; j++)
                {
                    A[k, j] -= factor * A[i, j];
                }
            }
        }

        for (int i = n - 1; i >= 0; i--)
        {
            for (int j = i + 1; j < n; j++)
            {
                A[i, n] -= A[i, j] * A[j, n];
            }
            A[i, n] /= A[i, i];
        }
    }
}
