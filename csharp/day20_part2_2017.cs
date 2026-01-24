using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

struct Particle
{
    public int[] p, v, a;
    public bool alive;
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var particles = new List<Particle>();
        var regex = new Regex(@"-?\d+");
        foreach (var line in lines)
        {
            var m = regex.Matches(line);
            var p = new Particle
            {
                p = new[] { int.Parse(m[0].Value), int.Parse(m[1].Value), int.Parse(m[2].Value) },
                v = new[] { int.Parse(m[3].Value), int.Parse(m[4].Value), int.Parse(m[5].Value) },
                a = new[] { int.Parse(m[6].Value), int.Parse(m[7].Value), int.Parse(m[8].Value) },
                alive = true
            };
            particles.Add(p);
        }

        for (int step = 0; step < 1000; step++)
        {
            for (int i = 0; i < particles.Count; i++)
            {
                if (!particles[i].alive) continue;
                var p = particles[i];
                p.v[0] += p.a[0];
                p.v[1] += p.a[1];
                p.v[2] += p.a[2];
                p.p[0] += p.v[0];
                p.p[1] += p.v[1];
                p.p[2] += p.v[2];
                particles[i] = p;
            }

            var posMap = new Dictionary<string, List<int>>();
            for (int i = 0; i < particles.Count; i++)
            {
                if (!particles[i].alive) continue;
                var key = $"{particles[i].p[0]},{particles[i].p[1]},{particles[i].p[2]}";
                if (!posMap.TryGetValue(key, out var list)) posMap[key] = list = new List<int>();
                list.Add(i);
            }

            foreach (var kv in posMap)
            {
                if (kv.Value.Count > 1)
                    foreach (var idx in kv.Value)
                        particles[idx] = new Particle { p = particles[idx].p, v = particles[idx].v, a = particles[idx].a, alive = false };
            }
        }

        int count = 0;
        foreach (var p in particles) if (p.alive) count++;
        Console.WriteLine(count);
    }
}
