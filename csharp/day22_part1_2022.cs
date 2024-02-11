
using System;
using System.IO;
using System.Collections.Generic;

public class Program
{
    public struct P
    {
        public int X, Y;
    }

    public enum Dir
    {
        N,
        E,
        S,
        W
    }

    public static Dir Rotate(Dir dir, char direction)
    {
        switch (direction)
        {
            case 'R':
                return (Dir)(((int)dir + 1) % 4);
            case 'L':
                return (Dir)(((int)dir - 1 + 4) % 4);
        }
        return dir;
    }

    public static int Points(Dir dir)
    {
        return ((int)dir + 3) % 4;
    }

    public struct Movement
    {
        public int Steps;
        public char Rotate;
    }

    public static Dictionary<P, bool> Map = new Dictionary<P, bool>();
    public static int Size;
    public static List<Movement> Movements = new List<Movement>();
    public static P[] Dirs = new P[]
    {
        new P { X = -1, Y = 0 }, // N
        new P { X = 0, Y = 1 },  // E
        new P { X = 1, Y = 0 },  // S
        new P { X = 0, Y = -1 }  // W
    };

    public struct Human
    {
        public P Curr;
        public Dir Facing;
    }

    public static void Main()
    {
        Parse();

        Human human = new Human
        {
            Curr = new P { X = 0, Y = Size },
            Facing = Dir.E
        };

        foreach (var mov in Movements)
        {
            human.Facing = Rotate(human.Facing, mov.Rotate);
            for (int i = 0; i < mov.Steps; i++)
            {
                if (!Walk(ref human))
                {
                    break;
                }
            }
        }

        Console.WriteLine(1000 * (human.Curr.X + 1) + 4 * (human.Curr.Y + 1) + Points(human.Facing));
    }

    public static void Parse()
    {
        using (StreamReader reader = new StreamReader("input.txt"))
        {
            string line;
            int r = 0;
            while ((line = reader.ReadLine()) != null)
            {
                if (line == "")
                {
                    break;
                }

                if (r == 0)
                {
                    Size = line.Length / 3;
                }

                for (int c = 0; c < line.Length; c++)
                {
                    char character = line[c];

                    switch (character)
                    {
                        case ' ':
                            continue;
                        case '#':
                            Map[new P { X = r, Y = c }] = true;
                            break;
                        case '.':
                            Map[new P { X = r, Y = c }] = false;
                            break;
                    }
                }
                r++;
            }

            string path = reader.ReadLine();
            Movements = ParsePath(path);
        }
    }

    public static List<Movement> ParsePath(string path)
    {
        List<Movement> movements = new List<Movement>();
        int acc = 0;
        foreach (char c in path)
        {
            switch (c)
            {
                case 'R':
                    movements.Add(new Movement { Steps = acc });
                    acc = 0;
                    movements.Add(new Movement { Rotate = 'R' });
                    break;
                case 'L':
                    movements.Add(new Movement { Steps = acc });
                    acc = 0;
                    movements.Add(new Movement { Rotate = 'L' });
                    break;
                default:
                    acc = 10 * acc + (int)(c - '0');
                    break;
            }
        }
        movements.Add(new Movement { Steps = acc });
        return movements;
    }

    public static bool Walk(ref Human h)
    {
        P dir = Dirs[(int)h.Facing];

        P next = new P { X = h.Curr.X + dir.X, Y = h.Curr.Y + dir.Y };
        if (Map.ContainsKey(next))
        {
            if (Map[next])
            {
                return false;
            }
            h.Curr = next;
            return true;
        }

        P oppDir = new P { X = -dir.X, Y = -dir.Y };
        while (true)
        {
            P lookAhead = new P { X = next.X + oppDir.X, Y = next.Y + oppDir.Y };
            if (!Map.ContainsKey(lookAhead))
            {
                if (Map[next])
                {
                    return false;
                }
                h.Curr = next;
                return true;
            }
            next = lookAhead;
        }
    }
}
