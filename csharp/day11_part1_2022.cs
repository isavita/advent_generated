
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public enum OperationType
{
    Add,
    Multiply,
    Square
}

public class Monkey
{
    public List<ulong> Items { get; set; }
    public OperationType Operation { get; set; }
    public int OperationValue { get; set; }
    public int TestDivisor { get; set; }
    public int TrueTarget { get; set; }
    public int FalseTarget { get; set; }
    public ulong Inspections { get; set; }

    public Monkey()
    {
        Items = new List<ulong>();
    }
}

class Program
{
    static void Main(string[] args)
    {
        var monkeys = ParseMonkeys("input.txt");
        SimulateRounds(monkeys, 20);
        var monkeyBusiness = CalculateMonkeyBusiness(monkeys);
        Console.WriteLine(monkeyBusiness);
    }

    static List<Monkey> ParseMonkeys(string filePath)
    {
        var monkeys = new List<Monkey>();
        var lines = File.ReadAllLines(filePath);

        for (var i = 0; i < lines.Length; i++)
        {
            if (lines[i].StartsWith("Monkey"))
            {
                var monkey = new Monkey();
                // Parse starting items
                var itemsLine = lines[i + 1].Trim();
                var items = itemsLine.Substring(itemsLine.IndexOf(":") + 1).Trim().Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
                monkey.Items.AddRange(items.Select(item => ulong.Parse(item.Trim())));

                // Parse operation
                var operationLine = lines[i + 2].Trim();
                var operationParts = operationLine.Substring(operationLine.IndexOf("=") + 1).Trim().Split(' ');
                var operand1 = operationParts[0];
                var op = operationParts[1];
                var operand2 = operationParts[2];

                if (op == "+")
                {
                    monkey.Operation = OperationType.Add;
                    monkey.OperationValue = operand2 == "old" ? -1 : int.Parse(operand2);
                }
                else if (op == "*")
                {
                    if (operand2 == "old")
                    {
                        monkey.Operation = OperationType.Square;
                    }
                    else
                    {
                        monkey.Operation = OperationType.Multiply;
                        monkey.OperationValue = int.Parse(operand2);
                    }
                }

                // Parse test
                var testLine = lines[i + 3].Trim();
                monkey.TestDivisor = int.Parse(testLine.Substring(testLine.LastIndexOf(" ") + 1));

                // Parse true and false targets
                var trueLine = lines[i + 4].Trim();
                monkey.TrueTarget = int.Parse(trueLine.Substring(trueLine.LastIndexOf(" ") + 1));

                var falseLine = lines[i + 5].Trim();
                monkey.FalseTarget = int.Parse(falseLine.Substring(falseLine.LastIndexOf(" ") + 1));

                monkeys.Add(monkey);
                i += 5; // Skip to the next monkey
            }
        }

        return monkeys;
    }

    static void SimulateRounds(List<Monkey> monkeys, int rounds)
    {
        for (var r = 0; r < rounds; r++)
        {
            foreach (var monkey in monkeys)
            {
                while (monkey.Items.Count > 0)
                {
                    var item = monkey.Items[0];
                    monkey.Items.RemoveAt(0);
                    monkey.Inspections++;

                    ulong newItem = 0;
                    switch (monkey.Operation)
                    {
                        case OperationType.Add:
                            newItem = (ulong)((monkey.OperationValue == -1) ? (long)item + (long)item : (long)item + monkey.OperationValue);
                            break;
                        case OperationType.Multiply:
                            newItem = item * (ulong)monkey.OperationValue;
                            break;
                        case OperationType.Square:
                            newItem = item * item;
                            break;
                    }

                    newItem /= 3;

                    var targetMonkey = (newItem % (ulong)monkey.TestDivisor == 0) ? monkey.TrueTarget : monkey.FalseTarget;
                    monkeys[targetMonkey].Items.Add(newItem);
                }
            }
        }
    }

    static ulong CalculateMonkeyBusiness(List<Monkey> monkeys)
    {
        var inspections = monkeys.Select(m => m.Inspections).OrderByDescending(i => i).ToList();
        return inspections.Count >= 2 ? inspections[0] * inspections[1] : inspections[0];
    }
}
