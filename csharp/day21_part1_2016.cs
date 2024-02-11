
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] operations = File.ReadAllLines("input.txt");
        string password = "abcdefgh";

        foreach (var op in operations)
        {
            password = ApplyOperation(op, password);
        }

        Console.WriteLine(password);
    }

    static string ApplyOperation(string op, string password)
    {
        var fields = op.Split(' ');
        switch (fields[0])
        {
            case "swap":
                if (fields[1] == "position")
                {
                    int x = int.Parse(fields[2]);
                    int y = int.Parse(fields[5]);
                    password = SwapPosition(password, x, y);
                }
                else if (fields[1] == "letter")
                {
                    char x = fields[2][0];
                    char y = fields[5][0];
                    password = SwapLetter(password, x, y);
                }
                break;
            case "rotate":
                if (fields[1] == "left")
                {
                    int steps = int.Parse(fields[2]);
                    password = RotateLeft(password, steps);
                }
                else if (fields[1] == "right")
                {
                    int steps = int.Parse(fields[2]);
                    password = RotateRight(password, steps);
                }
                else if (fields[1] == "based")
                {
                    char x = fields[6][0];
                    password = RotateBasedOnPosition(password, x);
                }
                break;
            case "reverse":
                int x = int.Parse(fields[2]);
                int y = int.Parse(fields[4]);
                password = ReversePositions(password, x, y);
                break;
            case "move":
                int a = int.Parse(fields[2]);
                int b = int.Parse(fields[5]);
                password = MovePosition(password, a, b);
                break;
        }
        return password;
    }

    static string SwapPosition(string password, int x, int y)
    {
        char[] chars = password.ToCharArray();
        char temp = chars[x];
        chars[x] = chars[y];
        chars[y] = temp;
        return new string(chars);
    }

    static string SwapLetter(string password, char x, char y)
    {
        return new string(password.Select(c => c == x ? y : c == y ? x : c).ToArray());
    }

    static string RotateLeft(string password, int steps)
    {
        steps = steps % password.Length;
        return password.Substring(steps) + password.Substring(0, steps);
    }

    static string RotateRight(string password, int steps)
    {
        steps = steps % password.Length;
        return password.Substring(password.Length - steps) + password.Substring(0, password.Length - steps);
    }

    static string RotateBasedOnPosition(string password, char x)
    {
        int index = password.IndexOf(x);
        int steps = 1 + index + (index >= 4 ? 1 : 0);
        return RotateRight(password, steps);
    }

    static string ReversePositions(string password, int x, int y)
    {
        char[] chars = password.ToCharArray();
        Array.Reverse(chars, x, y - x + 1);
        return new string(chars);
    }

    static string MovePosition(string password, int x, int y)
    {
        char c = password[x];
        string result = password.Remove(x, 1);
        return result.Insert(y, c.ToString());
    }
}
