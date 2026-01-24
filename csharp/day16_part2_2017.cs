
using System;
using System.IO;

class Program
{
    const int Size = 16;
    const long Iterations = 1_000_000_000L;

    enum MoveType { Spin, Exchange, Partner }

    struct Move
    {
        public MoveType Type;
        public int X, A, B;
        public char C1, C2;
    }

    static void Spin(char[] p, int x)
    {
        x %= Size;
        if (x == 0) return;
        var tmp = new char[x];
        Array.Copy(p, Size - x, tmp, 0, x);
        Array.Copy(p, 0, p, x, Size - x);
        Array.Copy(tmp, 0, p, 0, x);
    }

    static void Exchange(char[] p, int a, int b)
    {
        var t = p[a];
        p[a] = p[b];
        p[b] = t;
    }

    static void Partner(char[] p, char a, char b)
    {
        int ia = -1, ib = -1;
        for (int i = 0; i < Size; i++)
        {
            if (p[i] == a) ia = i;
            if (p[i] == b) ib = i;
        }
        if (ia != -1 && ib != -1) Exchange(p, ia, ib);
    }

    static void Apply(char[] p, Move m)
    {
        switch (m.Type)
        {
            case MoveType.Spin: Spin(p, m.X); break;
            case MoveType.Exchange: Exchange(p, m.A, m.B); break;
            case MoveType.Partner: Partner(p, m.C1, m.C2); break;
        }
    }

    static Move Parse(string token)
    {
        var move = new Move();
        switch (token[0])
        {
            case 's':
                move.Type = MoveType.Spin;
                move.X = int.Parse(token.Substring(1));
                break;
            case 'x':
                move.Type = MoveType.Exchange;
                var partsX = token.Substring(1).Split('/');
                move.A = int.Parse(partsX[0]);
                move.B = int.Parse(partsX[1]);
                break;
            case 'p':
                move.Type = MoveType.Partner;
                move.C1 = token[1];
                move.C2 = token[3];
                break;
        }
        return move;
    }

    static void Main()
    {
        var line = File.ReadAllText("input.txt").Trim();
        var tokens = line.Split(',');

        var moves = new Move[tokens.Length];
        for (int i = 0; i < tokens.Length; i++) moves[i] = Parse(tokens[i]);

        var programs = "abcdefghijklmnop".ToCharArray();
        var initial = (char[])programs.Clone();

        long cycle = 0;
        for (long i = 0; i < Iterations; i++)
        {
            foreach (var m in moves) Apply(programs, m);
            if (programs.AsSpan().SequenceEqual(initial))
            {
                cycle = i + 1;
                break;
            }
        }

        if (cycle > 0)
        {
            var remaining = (int)(Iterations % cycle);
            programs = (char[])initial.Clone();
            for (int i = 0; i < remaining; i++)
                foreach (var m in moves) Apply(programs, m);
        }

        Console.WriteLine(new string(programs));
    }
}
