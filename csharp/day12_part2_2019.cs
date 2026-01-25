
using System;
using System.IO;

public struct Vec3
{
    public int X, Y, Z;
}

public struct Moon
{
    public Vec3 Pos, Vel;
}

class Program
{
    static void ApplyGravity(Moon[] moons, char axis)
    {
        for (int i = 0; i < moons.Length; i++)
        {
            for (int j = i + 1; j < moons.Length; j++)
            {
                if (axis == 'x')
                {
                    if (moons[i].Pos.X > moons[j].Pos.X)
                    {
                        moons[i].Vel.X--;
                        moons[j].Vel.X++;
                    }
                    else if (moons[i].Pos.X < moons[j].Pos.X)
                    {
                        moons[i].Vel.X++;
                        moons[j].Vel.X--;
                    }
                }
                else if (axis == 'y')
                {
                    if (moons[i].Pos.Y > moons[j].Pos.Y)
                    {
                        moons[i].Vel.Y--;
                        moons[j].Vel.Y++;
                    }
                    else if (moons[i].Pos.Y < moons[j].Pos.Y)
                    {
                        moons[i].Vel.Y++;
                        moons[j].Vel.Y--;
                    }
                }
                else if (axis == 'z')
                {
                    if (moons[i].Pos.Z > moons[j].Pos.Z)
                    {
                        moons[i].Vel.Z--;
                        moons[j].Vel.Z++;
                    }
                    else if (moons[i].Pos.Z < moons[j].Pos.Z)
                    {
                        moons[i].Vel.Z++;
                        moons[j].Vel.Z--;
                    }
                }
            }
        }
    }

    static void ApplyVelocity(Moon[] moons, char axis)
    {
        for (int i = 0; i < moons.Length; i++)
        {
            if (axis == 'x') moons[i].Pos.X += moons[i].Vel.X;
            else if (axis == 'y') moons[i].Pos.Y += moons[i].Vel.Y;
            else if (axis == 'z') moons[i].Pos.Z += moons[i].Vel.Z;
        }
    }

    static long FindCycle(Moon[] moons, Moon[] initialMoons, char axis)
    {
        long steps = 0;
        while (true)
        {
            steps++;
            ApplyGravity(moons, axis);
            ApplyVelocity(moons, axis);
            bool match = true;
            for (int i = 0; i < moons.Length; i++)
            {
                if ((axis == 'x' && (moons[i].Pos.X != initialMoons[i].Pos.X || moons[i].Vel.X != initialMoons[i].Vel.X)) ||
                    (axis == 'y' && (moons[i].Pos.Y != initialMoons[i].Pos.Y || moons[i].Vel.Y != initialMoons[i].Vel.Y)) ||
                    (axis == 'z' && (moons[i].Pos.Z != initialMoons[i].Pos.Z || moons[i].Vel.Z != initialMoons[i].Vel.Z)))
                {
                    match = false;
                    break;
                }
            }
            if (match) return steps;
        }
    }

    static long GCD(long a, long b)
    {
        while (b != 0)
        {
            long t = b;
            b = a % b;
            a = t;
        }
        return a;
    }

    static long LCM(long a, long b)
    {
        return (a / GCD(a, b)) * b;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Moon[] moons = new Moon[lines.Length];
        Moon[] initialMoons = new Moon[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            string[] parts = lines[i].Trim('<', '>', '\n').Split(',');
            string[] x = parts[0].Trim().Split('=');
            string[] y = parts[1].Trim().Split('=');
            string[] z = parts[2].Trim().Split('=');

            moons[i].Pos.X = int.Parse(x[1]);
            moons[i].Pos.Y = int.Parse(y[1]);
            moons[i].Pos.Z = int.Parse(z[1]);

            moons[i].Vel = new Vec3 { X = 0, Y = 0, Z = 0 };
            initialMoons[i] = moons[i];
        }

        long cycleX = FindCycle((Moon[])moons.Clone(), (Moon[])initialMoons.Clone(), 'x');
        long cycleY = FindCycle((Moon[])moons.Clone(), (Moon[])initialMoons.Clone(), 'y');
        long cycleZ = FindCycle((Moon[])moons.Clone(), (Moon[])initialMoons.Clone(), 'z');

        long result = LCM(LCM(cycleX, cycleY), cycleZ);
        Console.WriteLine(result);
    }
}
