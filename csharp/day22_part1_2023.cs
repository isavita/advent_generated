
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Coord
{
    public int x;
    public int y;
    public int z;
}

class Brick
{
    public Coord mini = new Coord();
    public Coord maxi = new Coord();
    public List<Brick> basedOn = new List<Brick>();
    public List<Brick> support = new List<Brick>();
}

class Program
{
    static List<Brick> ParseInput(string[] input)
    {
        var bricks = new List<Brick>();
        foreach (var line in input)
        {
            var brick = new Brick();
            var values = line.Split('~');
            var miniValues = values[0].Split(',');
            var maxiValues = values[1].Split(',');
            brick.mini.x = int.Parse(miniValues[0]);
            brick.mini.y = int.Parse(miniValues[1]);
            brick.mini.z = int.Parse(miniValues[2]);
            brick.maxi.x = int.Parse(maxiValues[0]);
            brick.maxi.y = int.Parse(maxiValues[1]);
            brick.maxi.z = int.Parse(maxiValues[2]);
            bricks.Add(brick);
        }
        return bricks;
    }

    static void Settle(List<Brick> bricks)
    {
        bricks = bricks.OrderBy(b => b.maxi.z).ToList();

        for (int i = 0; i < bricks.Count; i++)
        {
            int supportZ = 0;
            var basedBricks = new List<Brick>();

            for (int j = i - 1; j > -1; j--)
            {
                bool isIntersectingX = Math.Max(bricks[i].mini.x, bricks[j].mini.x) <= Math.Min(bricks[i].maxi.x, bricks[j].maxi.x);
                bool isIntersectingY = Math.Max(bricks[i].mini.y, bricks[j].mini.y) <= Math.Min(bricks[i].maxi.y, bricks[j].maxi.y);
                bool isIntersecting = isIntersectingX && isIntersectingY;
                if (isIntersecting)
                {
                    if (bricks[j].maxi.z == supportZ)
                    {
                        basedBricks.Add(bricks[j]);
                    }
                    else if (bricks[j].maxi.z > supportZ)
                    {
                        supportZ = bricks[j].maxi.z;
                        basedBricks = new List<Brick> { bricks[j] };
                    }
                }
            }

            bricks[i].basedOn = basedBricks;
            foreach (var basedBrick in basedBricks)
            {
                basedBrick.support.Add(bricks[i]);
            }

            int deltaZ = bricks[i].maxi.z - bricks[i].mini.z;
            bricks[i].mini.z = supportZ + 1;
            bricks[i].maxi.z = bricks[i].mini.z + deltaZ;
        }
    }

    static int Solve(string[] input)
    {
        var bricks = ParseInput(input);
        Settle(bricks);

        int cnt = 0;
        foreach (var brick in bricks)
        {
            bool isDisintegratable = true;
            foreach (var supportedBrick in brick.support)
            {
                if (supportedBrick.basedOn.Count < 2)
                {
                    isDisintegratable = false;
                    break;
                }
            }
            if (isDisintegratable)
            {
                cnt++;
            }
        }
        return cnt;
    }

    static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    static void Main()
    {
        var input = ReadFile("input.txt");
        Console.WriteLine(Solve(input));
    }
}
