
using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

class Bot
{
    public string LowTo { get; set; }
    public string HighTo { get; set; }
    public List<int> Chips { get; set; }

    public Bot()
    {
        Chips = new List<int>();
    }
}

class Program
{
    static void Main()
    {
        Dictionary<string, Bot> bots = new Dictionary<string, Bot>();
        Dictionary<string, int> outputs = new Dictionary<string, int>();
        Regex valueRegex = new Regex(@"value (\d+) goes to (bot \d+)");
        Regex givesRegex = new Regex(@"(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)");

        using (StreamReader file = new StreamReader("input.txt"))
        {
            string line;
            while ((line = file.ReadLine()) != null)
            {
                if (valueRegex.IsMatch(line))
                {
                    Match match = valueRegex.Match(line);
                    int value = int.Parse(match.Groups[1].Value);
                    string botID = match.Groups[2].Value;

                    if (!bots.ContainsKey(botID))
                    {
                        bots[botID] = new Bot();
                    }
                    bots[botID].Chips.Add(value);
                }
                else if (givesRegex.IsMatch(line))
                {
                    Match match = givesRegex.Match(line);
                    string botID = match.Groups[1].Value;
                    string lowTo = match.Groups[2].Value;
                    string highTo = match.Groups[3].Value;

                    if (!bots.ContainsKey(botID))
                    {
                        bots[botID] = new Bot();
                    }
                    bots[botID].LowTo = lowTo;
                    bots[botID].HighTo = highTo;
                }
            }
        }

        while (true)
        {
            bool action = false;
            foreach (var b in bots.Values)
            {
                if (b.Chips.Count == 2)
                {
                    action = true;
                    int low = Math.Min(b.Chips[0], b.Chips[1]);
                    int high = Math.Max(b.Chips[0], b.Chips[1]);
                    b.Chips.Clear();

                    GiveChip(bots, outputs, b.LowTo, low);
                    GiveChip(bots, outputs, b.HighTo, high);
                }
            }
            if (!action)
            {
                break;
            }
        }

        int result = outputs["output 0"] * outputs["output 1"] * outputs["output 2"];
        Console.WriteLine(result);
    }

    static void GiveChip(Dictionary<string, Bot> bots, Dictionary<string, int> outputs, string target, int value)
    {
        if (target.StartsWith("bot"))
        {
            if (!bots.ContainsKey(target))
            {
                bots[target] = new Bot();
            }
            bots[target].Chips.Add(value);
        }
        else if (target.StartsWith("output"))
        {
            outputs[target] = value;
        }
    }
}
