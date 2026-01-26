using System;
using System.IO;
using System.Collections.Generic;
using System.Text;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var wireValues = new Dictionary<string, int>();
        var gateInstructions = new List<string>();
        bool initialSection = true;
        foreach (var raw in File.ReadLines("input.txt"))
        {
            var line = raw.Trim();
            if (line.Length == 0) continue;
            if (initialSection && line.Contains(":"))
            {
                var parts = line.Split(':');
                wireValues[parts[0].Trim()] = int.Parse(parts[1].Trim());
            }
            else
            {
                initialSection = false;
                gateInstructions.Add(line);
            }
        }

        var calculated = new Dictionary<string, int>(wireValues);
        var gateRegex = new Regex(@"([a-z0-9]+)\s(AND|OR|XOR)\s([a-z0-9]+)\s->\s([a-z0-9]+)");
        var copyRegex = new Regex(@"([a-z0-9]+)\s->\s([a-z0-9]+)");

        while (true)
        {
            bool changed = false;
            foreach (var instr in gateInstructions)
            {
                var outWire = instr.Split("->", 2)[1].Trim();
                if (calculated.ContainsKey(outWire)) continue;

                var m = gateRegex.Match(instr);
                if (m.Success)
                {
                    var in1 = m.Groups[1].Value;
                    var op = m.Groups[2].Value;
                    var in2 = m.Groups[3].Value;
                    var outName = m.Groups[4].Value;
                    if (calculated.ContainsKey(in1) && calculated.ContainsKey(in2))
                    {
                        int v1 = calculated[in1], v2 = calculated[in2], res = 0;
                        switch (op)
                        {
                            case "AND": res = v1 & v2; break;
                            case "OR":  res = v1 | v2; break;
                            case "XOR": res = v1 ^ v2; break;
                        }
                        calculated[outName] = res;
                        changed = true;
                    }
                }
                else
                {
                    var m2 = copyRegex.Match(instr);
                    if (m2.Success)
                    {
                        var inWire = m2.Groups[1].Value;
                        var outName = m2.Groups[2].Value;
                        if (calculated.ContainsKey(inWire))
                        {
                            calculated[outName] = calculated[inWire];
                            changed = true;
                        }
                    }
                }
            }
            if (!changed) break;
        }

        var sb = new StringBuilder();
        for (int i = 0; ; i++)
        {
            var key = $"z{i:D2}";
            if (!calculated.ContainsKey(key)) break;
            sb.Insert(0, calculated[key]);
        }

        long result = 0;
        if (sb.Length > 0 && long.TryParse(sb.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier, null, out _))
        {
            // Not needed, placeholder
        }
        if (sb.Length > 0) result = Convert.ToInt64(sb.ToString(), 2);
        Console.WriteLine(result);
    }
}