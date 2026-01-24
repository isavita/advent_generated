
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void SwapPos(char[] a, int x, int y) { var t = a[x]; a[x] = a[y]; a[y] = t; }
    static void SwapLetter(char[] a, char x, char y)
    {
        int ix = Array.IndexOf(a, x), iy = Array.IndexOf(a, y);
        if (ix >= 0 && iy >= 0) SwapPos(a, ix, iy);
    }
    static void Rotate(char[] a, int steps)
    {
        int n = a.Length;
        steps = ((steps % n) + n) % n;
        if (steps == 0) return;
        var tmp = new char[steps];
        Array.Copy(a, n - steps, tmp, 0, steps);
        Array.Copy(a, 0, a, steps, n - steps);
        Array.Copy(tmp, 0, a, 0, steps);
    }
    static void DerotateLetter(char[] a, char x)
    {
        int i = Array.IndexOf(a, x);
        int rot;
        if (i % 2 == 1) rot = -(i + 1) / 2;
        else if (i != 0) rot = (6 - i) / 2;
        else rot = -1;
        Rotate(a, rot);
    }
    static void Reverse(char[] a, int x, int y)
    {
        while (x < y) SwapPos(a, x++, y--);
    }
    static void Move(char[] a, int x, int y)
    {
        char c = a[x];
        if (x < y) Array.Copy(a, x + 1, a, x, y - x);
        else if (x > y) Array.Copy(a, y, a, y + 1, x - y);
        a[y] = c;
    }
    static void Unscramble(char[] pw, IList<string> ins)
    {
        for (int i = ins.Count - 1; i >= 0; i--)
        {
            var s = ins[i];
            if (s.StartsWith("swap position"))
            {
                var p = s.Split(' ');
                int x = int.Parse(p[2]), y = int.Parse(p[5]);
                SwapPos(pw, x, y);
            }
            else if (s.StartsWith("swap letter"))
            {
                var p = s.Split(' ');
                char x = p[2][0], y = p[5][0];
                SwapLetter(pw, x, y);
            }
            else if (s.StartsWith("rotate based"))
            {
                char x = s[s.Length - 1];
                DerotateLetter(pw, x);
            }
            else if (s.StartsWith("rotate "))
            {
                var p = s.Split(' ');
                int steps = int.Parse(p[2]);
                if (p[1] == "left") steps = -steps;
                Rotate(pw, -steps);
            }
            else if (s.StartsWith("reverse"))
            {
                var p = s.Split(' ');
                int x = int.Parse(p[2]), y = int.Parse(p[4]);
                Reverse(pw, x, y);
            }
            else if (s.StartsWith("move"))
            {
                var p = s.Split(' ');
                int x = int.Parse(p[2]), y = int.Parse(p[5]);
                Move(pw, y, x);
            }
        }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var pw = "fbgdceah".ToCharArray();
        Unscramble(pw, lines);
        Console.WriteLine(new string(pw));
    }
}
