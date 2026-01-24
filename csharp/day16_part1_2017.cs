using System;
using System.IO;

class Program
{
    static void Spin(char[] p, int x)
    {
        int n = p.Length;
        char[] t = new char[n];
        Array.Copy(p, t, n);
        for (int i = 0; i < n; i++) p[(i + x) % n] = t[i];
    }

    static void Exchange(char[] p, int a, int b)
    {
        char t = p[a];
        p[a] = p[b];
        p[b] = t;
    }

    static void Partner(char[] p, char a, char b)
    {
        int iA = -1, iB = -1;
        for (int i = 0; i < p.Length; i++)
        {
            if (p[i] == a) iA = i;
            if (p[i] == b) iB = i;
        }
        Exchange(p, iA, iB);
    }

    static void Main()
    {
        string line = File.ReadAllText("input.txt").Trim();
        char[] programs = "abcdefghijklmnop".ToCharArray();
        foreach (var move in line.Split(','))
        {
            switch (move[0])
            {
                case 's':
                    Spin(programs, int.Parse(move.Substring(1)));
                    break;
                case 'x':
                    var parts = move.Substring(1).Split('/');
                    Exchange(programs, int.Parse(parts[0]), int.Parse(parts[1]));
                    break;
                case 'p':
                    Partner(programs, move[1], move[3]);
                    break;
            }
        }
        Console.WriteLine(new string(programs));
    }
}