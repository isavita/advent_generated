
using System;
using System.Collections.Generic;
using System.IO;

class Solution
{
    static int GetCombo(int op, int a, int b, int c) =>
        op <= 3 ? op :
        op == 4 ? a :
        op == 5 ? b :
        op == 6 ? c :
        throw new ArgumentException();

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int A = 0, B = 0, C = 0;
        var prog = new List<int>();
        foreach (var raw in lines)
        {
            var line = raw.Trim();
            if (line.StartsWith("Register A:"))
                A = int.Parse(line.Split(':')[1].Trim());
            else if (line.StartsWith("Register B:"))
                B = int.Parse(line.Split(':')[1].Trim());
            else if (line.StartsWith("Register C:"))
                C = int.Parse(line.Split(':')[1].Trim());
            else if (line.StartsWith("Program:"))
            {
                var parts = line.Split(':')[1].Split(',');
                foreach (var p in parts) prog.Add(int.Parse(p.Trim()));
            }
        }

        var output = new List<int>();
        int ip = 0;
        while (ip + 1 < prog.Count)
        {
            int op = prog[ip];
            int operand = prog[ip + 1];
            switch (op)
            {
                case 0:
                    var den0 = GetCombo(operand, A, B, C);
                    A = den0 != 0 ? A >> den0 : 0;
                    ip += 2;
                    break;
                case 1:
                    B ^= operand;
                    ip += 2;
                    break;
                case 2:
                    B = GetCombo(operand, A, B, C) % 8;
                    ip += 2;
                    break;
                case 3:
                    ip = A != 0 ? operand : ip + 2;
                    break;
                case 4:
                    B ^= C;
                    ip += 2;
                    break;
                case 5:
                    output.Add(GetCombo(operand, A, B, C) % 8);
                    ip += 2;
                    break;
                case 6:
                    B = A >> GetCombo(operand, A, B, C);
                    ip += 2;
                    break;
                case 7:
                    C = A >> GetCombo(operand, A, B, C);
                    ip += 2;
                    break;
                default:
                    ip = prog.Count;
                    break;
            }
        }

        Console.WriteLine(string.Join(",", output));
    }
}
