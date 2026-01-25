using System;
using System.IO;

class Program
{
    static void Main()
    {
        var data = File.ReadAllText("input.txt");
        int pos = 50, cnt = 0;
        foreach (var token in data.Split((char[])null, StringSplitOptions.RemoveEmptyEntries))
        {
            char dir = token[0];
            int amt = int.Parse(token.AsSpan(1));
            pos = (pos + (dir == 'R' ? amt : -amt)) % 100;
            if (pos < 0) pos += 100;
            if (pos == 0) cnt++;
        }
        Console.WriteLine(cnt);
    }
}