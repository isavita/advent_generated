
using System;
using System.IO;

class Vec3
{
    public int x, y, z;
}

class Moon
{
    public Vec3 pos, vel;
}

class Program
{
    static int Abs(int x)
    {
        return x < 0 ? -x : x;
    }

    static void ApplyGravity(Moon[] moons)
    {
        for (int i = 0; i < moons.Length; i++)
        {
            for (int j = i + 1; j < moons.Length; j++)
            {
                if (moons[i].pos.x > moons[j].pos.x)
                {
                    moons[i].vel.x--;
                    moons[j].vel.x++;
                }
                else if (moons[i].pos.x < moons[j].pos.x)
                {
                    moons[i].vel.x++;
                    moons[j].vel.x--;
                }

                if (moons[i].pos.y > moons[j].pos.y)
                {
                    moons[i].vel.y--;
                    moons[j].vel.y++;
                }
                else if (moons[i].pos.y < moons[j].pos.y)
                {
                    moons[i].vel.y++;
                    moons[j].vel.y--;
                }

                if (moons[i].pos.z > moons[j].pos.z)
                {
                    moons[i].vel.z--;
                    moons[j].vel.z++;
                }
                else if (moons[i].pos.z < moons[j].pos.z)
                {
                    moons[i].vel.z++;
                    moons[j].vel.z--;
                }
            }
        }
    }

    static void ApplyVelocity(Moon[] moons)
    {
        foreach (var moon in moons)
        {
            moon.pos.x += moon.vel.x;
            moon.pos.y += moon.vel.y;
            moon.pos.z += moon.vel.z;
        }
    }

    static int TotalEnergy(Moon[] moons)
    {
        int total = 0;
        foreach (var m in moons)
        {
            int pot = Abs(m.pos.x) + Abs(m.pos.y) + Abs(m.pos.z);
            int kin = Abs(m.vel.x) + Abs(m.vel.y) + Abs(m.vel.z);
            total += pot * kin;
        }
        return total;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Moon[] moons = new Moon[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            int x, y, z;
            string[] parts = lines[i].Split(',', '=', '<', '>', ' ');
            x = int.Parse(parts[2]);
            y = int.Parse(parts[5]);
            z = int.Parse(parts[8]);
            moons[i] = new Moon { pos = new Vec3 { x = x, y = y, z = z }, vel = new Vec3 { x = 0, y = 0, z = 0 } };
        }

        for (int step = 0; step < 1000; step++)
        {
            ApplyGravity(moons);
            ApplyVelocity(moons);
        }

        Console.WriteLine(TotalEnergy(moons));
    }
}
