
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Dictionary<string, int> registers = new Dictionary<string, int>();

        foreach (string line in lines)
        {
            string[] parts = line.Split(' ');
            string reg = parts[0];
            string op = parts[1];
            int amount = int.Parse(parts[2]);
            string condReg = parts[4];
            string condOp = parts[5];
            int condVal = int.Parse(parts[6]);

            if (!registers.ContainsKey(reg))
            {
                registers[reg] = 0;
            }

            if (!registers.ContainsKey(condReg))
            {
                registers[condReg] = 0;
            }

            bool cond = false;
            switch (condOp)
            {
                case ">":
                    cond = registers[condReg] > condVal;
                    break;
                case ">=":
                    cond = registers[condReg] >= condVal;
                    break;
                case "<":
                    cond = registers[condReg] < condVal;
                    break;
                case "<=":
                    cond = registers[condReg] <= condVal;
                    break;
                case "==":
                    cond = registers[condReg] == condVal;
                    break;
                case "!=":
                    cond = registers[condReg] != condVal;
                    break;
            }

            if (cond)
            {
                switch (op)
                {
                    case "inc":
                        registers[reg] += amount;
                        break;
                    case "dec":
                        registers[reg] -= amount;
                        break;
                }
            }
        }

        int maxValue = 0;
        foreach (int value in registers.Values)
        {
            if (value > maxValue)
            {
                maxValue = value;
            }
        }

        Console.WriteLine(maxValue);
    }
}
