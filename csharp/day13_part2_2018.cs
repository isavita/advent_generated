using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Cart
{
    public int X, Y;
    public char Dir;
    public int Turns;
    public bool Removed;
    public void Move(char[][] track)
    {
        switch (Dir)
        {
            case '>': X++; break;
            case '<': X--; break;
            case '^': Y--; break;
            case 'v': Y++; break;
        }
        var cell = track[Y][X];
        if (cell == '+')
        {
            if (Turns % 3 == 0) Dir = Dir == '>' ? '^' : Dir == '<' ? 'v' : Dir == '^' ? '<' : '>';
            else if (Turns % 3 == 2) Dir = Dir == '>' ? 'v' : Dir == '<' ? '^' : Dir == '^' ? '>' : '<';
            Turns++;
        }
        else if (cell == '/')
        {
            Dir = Dir == '>' ? '^' : Dir == '<' ? 'v' : Dir == '^' ? '>' : '<';
        }
        else if (cell == '\\')
        {
            Dir = Dir == '>' ? 'v' : Dir == '<' ? '^' : Dir == '^' ? '<' : '>';
        }
    }
}

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var track = lines.Select(l => l.ToCharArray()).ToArray();
        var carts = new List<Cart>();
        for (int y = 0; y < track.Length; y++)
        {
            for (int x = 0; x < track[y].Length; x++)
            {
                var c = track[y][x];
                if ("<>^v".Contains(c))
                {
                    carts.Add(new Cart { X = x, Y = y, Dir = c, Turns = 0 });
                    track[y][x] = (c == '>' || c == '<') ? '-' : '|';
                }
            }
        }

        while (carts.Count > 1)
        {
            carts = carts.OrderBy(c => c.Y).ThenBy(c => c.X).ToList();
            foreach (var cart in carts)
            {
                if (cart.Removed) continue;
                cart.Move(track);
                var collided = carts.FirstOrDefault(c => !c.Removed && c != cart && c.X == cart.X && c.Y == cart.Y);
                if (collided != null)
                {
                    cart.Removed = true;
                    collided.Removed = true;
                }
            }
            carts = carts.Where(c => !c.Removed).ToList();
        }

        var last = carts[0];
        Console.WriteLine($"{last.X},{last.Y}");
    }
}
