
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    const int Floors = 4;
    struct State
    {
        public int Elevator;
        public int Steps;
        public int[] Gen; // index by material
        public int[] Chip;
        public State(int e, int[] g, int[] c, int s)
        {
            Elevator = e; Gen = g; Chip = c; Steps = s;
        }
        public string Key()
        {
            var pairs = new List<(int, int)>();
            for (int i = 0; i < Gen.Length; i++) pairs.Add((Gen[i], Chip[i]));
            pairs.Sort();
            var sb = new System.Text.StringBuilder();
            sb.Append(Elevator);
            foreach (var p in pairs) sb.Append('|').Append(p.Item1).Append(',').Append(p.Item2);
            return sb.ToString();
        }
    }

    static bool Valid(State st)
    {
        for (int f = 0; f < Floors; f++)
        {
            bool genPresent = false;
            var genSet = new HashSet<int>();
            for (int i = 0; i < st.Gen.Length; i++)
                if (st.Gen[i] == f) { genPresent = true; genSet.Add(i); }

            if (!genPresent) continue;

            for (int i = 0; i < st.Chip.Length; i++)
                if (st.Chip[i] == f && !genSet.Contains(i))
                    return false;
        }
        return true;
    }

    static bool Done(State st) => st.Gen.All(g => g == Floors - 1) && st.Chip.All(c => c == Floors - 1);

    static int Solve(string[] lines)
    {
        var materialId = new Dictionary<string, int>();
        var gen = new List<int>();
        var chip = new List<int>();
        for (int f = 0; f < Floors; f++)
        {
            var tokens = lines[f].Split(new[] { ' ', ',', '.' }, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < tokens.Length; i++)
            {
                if (tokens[i] == "generator")
                {
                    var name = tokens[i - 1];
                    if (!materialId.ContainsKey(name)) materialId[name] = materialId.Count;
                    int id = materialId[name];
                    while (gen.Count <= id) { gen.Add(0); chip.Add(0); }
                    gen[id] = f;
                }
                else if (tokens[i] == "microchip")
                {
                    var name = tokens[i - 1];
                    var dash = name.IndexOf('-');
                    if (dash >= 0) name = name.Substring(0, dash);
                    if (!materialId.ContainsKey(name)) materialId[name] = materialId.Count;
                    int id = materialId[name];
                    while (gen.Count <= id) { gen.Add(0); chip.Add(0); }
                    chip[id] = f;
                }
            }
        }
        var start = new State(0, gen.ToArray(), chip.ToArray(), 0);
        var q = new Queue<State>();
        var seen = new HashSet<string>();
        q.Enqueue(start);
        seen.Add(start.Key());

        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            if (Done(cur)) return cur.Steps;

            var items = new List<(bool isGen, int idx)>();
            for (int i = 0; i < cur.Gen.Length; i++)
                if (cur.Gen[i] == cur.Elevator) items.Add((true, i));
            for (int i = 0; i < cur.Chip.Length; i++)
                if (cur.Chip[i] == cur.Elevator) items.Add((false, i));

            var moves = new List<int[]>();
            for (int a = 0; a < items.Count; a++)
                moves.Add(new[] { a });
            for (int a = 0; a < items.Count; a++)
                for (int b = a + 1; b < items.Count; b++)
                    moves.Add(new[] { a, b });

            foreach (var dir in new[] { 1, -1 })
            {
                int nf = cur.Elevator + dir;
                if (nf < 0 || nf >= Floors) continue;
                foreach (var mv in moves)
                {
                    var ng = (int[])cur.Gen.Clone();
                    var nc = (int[])cur.Chip.Clone();
                    foreach (var idx in mv)
                    {
                        var it = items[idx];
                        if (it.isGen) ng[it.idx] = nf;
                        else nc[it.idx] = nf;
                    }
                    var nxt = new State(nf, ng, nc, cur.Steps + 1);
                    if (!Valid(nxt)) continue;
                    var key = nxt.Key();
                    if (seen.Add(key)) q.Enqueue(nxt);
                }
            }
        }
        return -1;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        Console.WriteLine(Solve(lines));
    }
}
