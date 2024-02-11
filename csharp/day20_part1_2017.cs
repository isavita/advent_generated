
using System;
using System.IO;
using System.Linq;

class Particle
{
    public int[] p = new int[3];
    public int[] v = new int[3];
    public int[] a = new int[3];
}

class Program
{
    static int Abs(int x)
    {
        return x < 0 ? -x : x;
    }

    static int Manhattan(int[] x)
    {
        return Abs(x[0]) + Abs(x[1]) + Abs(x[2]);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        var particles = lines.Select(line =>
        {
            var parts = line.Split(", ");

            var p = new Particle();
            for (int i = 0; i < parts.Length; i++)
            {
                var coords = parts[i][3..^1].Split(',');
                for (int j = 0; j < coords.Length; j++)
                {
                    int num = int.Parse(coords[j]);
                    switch (i)
                    {
                        case 0:
                            p.p[j] = num;
                            break;
                        case 1:
                            p.v[j] = num;
                            break;
                        case 2:
                            p.a[j] = num;
                            break;
                    }
                }
            }
            return p;
        }).ToList();

        int closestParticle = 0;
        int minAccel = int.MaxValue;
        int minVelocity = int.MaxValue;
        int minPosition = int.MaxValue;

        for (int i = 0; i < particles.Count; i++)
        {
            var particle = particles[i];
            int accel = Manhattan(particle.a);
            int velocity = Manhattan(particle.v);
            int position = Manhattan(particle.p);

            if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
                (accel == minAccel && velocity == minVelocity && position < minPosition))
            {
                minAccel = accel;
                minVelocity = velocity;
                minPosition = position;
                closestParticle = i;
            }
        }

        Console.WriteLine(closestParticle);
    }
}
