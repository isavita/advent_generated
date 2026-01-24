
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int HASH_TABLE_SIZE = 256;

    struct Lens
    {
        public string Label;
        public int FocalLength;
    }

    static int HashString(string str)
    {
        int res = 0;
        foreach (char c in str)
        {
            res += c;
            res *= 17;
            res %= HASH_TABLE_SIZE;
        }
        return res;
    }

    static void ProcessStep(Dictionary<int, List<Lens>> boxes, string step)
    {
        int opPos = step.IndexOfAny(new char[] { '=', '-' });
        string label = step.Substring(0, opPos);
        int boxId = HashString(label);
        char op = step[opPos];

        if (!boxes.ContainsKey(boxId))
            boxes[boxId] = new List<Lens>();

        var box = boxes[boxId];
        int index = box.FindIndex(l => l.Label == label);

        if (op == '-')
        {
            if (index >= 0)
                box.RemoveAt(index);
        }
        else
        {
            int focalLength = int.Parse(step.Substring(opPos + 1));
            if (index >= 0)
            {
                Lens l = box[index];
                l.FocalLength = focalLength;
                box[index] = l;
            }
            else
            {
                box.Add(new Lens { Label = label, FocalLength = focalLength });
            }
        }
    }

    static int CalculatePower(Dictionary<int, List<Lens>> boxes)
    {
        int res = 0;
        foreach (var kv in boxes)
        {
            for (int slot = 0; slot < kv.Value.Count; slot++)
            {
                res += (kv.Key + 1) * (slot + 1) * kv.Value[slot].FocalLength;
            }
        }
        return res;
    }

    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        Dictionary<int, List<Lens>> boxes = new Dictionary<int, List<Lens>>();
        foreach (string step in input.Split(','))
            ProcessStep(boxes, step);
        Console.WriteLine(CalculatePower(boxes));
    }
}
