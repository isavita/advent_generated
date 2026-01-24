using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string molecule = "";
        for (int i = 0; i < lines.Length; i++)
        {
            if (string.IsNullOrWhiteSpace(lines[i]) && i + 1 < lines.Length)
            {
                molecule = lines[i + 1].Trim();
                break;
            }
        }
        if (molecule.Length == 0) return;

        int total = 0, rn = 0, ar = 0, y = 0;
        ReadOnlySpan<char> span = molecule.AsSpan();
        for (int i = 0; i < span.Length; )
        {
            total++;
            int len = (i + 1 < span.Length && char.IsLower(span[i + 1])) ? 2 : 1;
            var elem = span.Slice(i, len);
            if (len == 2)
            {
                if (elem[0] == 'R' && elem[1] == 'n') rn++;
                else if (elem[0] == 'A' && elem[1] == 'r') ar++;
            }
            else
            {
                if (elem[0] == 'Y') y++;
            }
            i += len;
        }

        int steps = total - rn - ar - 2 * y - 1;
        Console.WriteLine(steps);
    }
}
