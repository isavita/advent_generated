using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int MAX_NAME_LEN = 8;
    const int NUM_PAIRS = 4;

    struct GateDef
    {
        public string a;
        public string op;
        public string b;
    }

    class Gate
    {
        public GateDef def;
        public string output;
    }

    static List<Gate> gates = new List<Gate>();

    static string FindOutputByGate(string a1, string op, string b1)
    {
        foreach (var g in gates)
        {
            if (g.def.op == op)
            {
                if ((g.def.a == a1 && g.def.b == b1) || (g.def.a == b1 && g.def.b == a1))
                    return g.output;
            }
        }
        return null;
    }

    static Gate FindGateByOutput(string output)
    {
        foreach (var g in gates)
            if (g.output == output) return g;
        return null;
    }

    static void SwapGateOutputs(string out1, string out2)
    {
        foreach (var g in gates)
        {
            if (g.output == out1) g.output = out2;
            else if (g.output == out2) g.output = out1;
        }
    }

    static void Main()
    {
        string buffer = File.ReadAllText("input.txt");
        int idx = buffer.IndexOf("\n\n", StringComparison.Ordinal);
        string definitions = idx >= 0 ? buffer.Substring(idx + 2) : buffer;

        int numZ = 0;
        foreach (var rawLine in definitions.Split('\n'))
        {
            string line = rawLine.Trim('\r');
            if (line.Length == 0) continue;
            var parts = line.Split(' ', StringSplitOptions.RemoveEmptyEntries);
            var g = new Gate();
            g.def.a = parts[0];
            g.def.op = parts[1];
            g.def.b = parts[2];
            g.output = parts[4];
            gates.Add(g);
            if (g.output.StartsWith('z')) numZ++;
        }

        var swappedPairs = new List<string>(NUM_PAIRS * 2);
        int pairCount = 0;

        while (pairCount < NUM_PAIRS)
        {
            string carry = "";
            for (int i = 0; i < numZ; i++)
            {
                string xi = $"x{i:D2}";
                string yi = $"y{i:D2}";
                string zi = $"z{i:D2}";

                string adder = null, nextCarry = null;

                if (i == 0)
                {
                    adder = FindOutputByGate(xi, "XOR", yi);
                    nextCarry = FindOutputByGate(xi, "AND", yi);
                }
                else
                {
                    string bit = FindOutputByGate(xi, "XOR", yi);
                    if (bit != null && !string.IsNullOrEmpty(carry))
                    {
                        adder = FindOutputByGate(bit, "XOR", carry);
                        if (adder != null)
                        {
                            string c1 = FindOutputByGate(xi, "AND", yi);
                            string c2 = FindOutputByGate(bit, "AND", carry);
                            if (c1 != null && c2 != null)
                                nextCarry = FindOutputByGate(c1, "OR", c2);
                        }
                    }
                }

                bool swapped = false;
                if (adder != null && adder != zi)
                {
                    swappedPairs.Add(adder);
                    swappedPairs.Add(zi);
                    SwapGateOutputs(adder, zi);
                    swapped = true;
                }
                else if (adder == null)
                {
                    var gateZ = FindGateByOutput(zi);
                    var bit = FindOutputByGate(xi, "XOR", yi);
                    if (gateZ != null && bit != null && !string.IsNullOrEmpty(carry))
                    {
                        if (FindOutputByGate(gateZ.def.a, "XOR", carry) != null)
                        {
                            swappedPairs.Add(bit);
                            swappedPairs.Add(gateZ.def.a);
                            SwapGateOutputs(bit, gateZ.def.a);
                            swapped = true;
                        }
                        else if (FindOutputByGate(gateZ.def.b, "XOR", carry) != null)
                        {
                            swappedPairs.Add(bit);
                            swappedPairs.Add(gateZ.def.b);
                            SwapGateOutputs(bit, gateZ.def.b);
                            swapped = true;
                        }
                    }
                }

                if (swapped)
                {
                    pairCount++;
                    break;
                }
                carry = nextCarry ?? "";
            }
        }

        swappedPairs.Sort(StringComparer.Ordinal);
        for (int i = 0; i < swappedPairs.Count; i++)
        {
            Console.Write(swappedPairs[i]);
            if (i != swappedPairs.Count - 1) Console.Write(",");
        }
        Console.WriteLine();
    }
}
