using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    const int MAX_MATERIALS = 10;
    const int NUM_FLOORS = 4;
    struct State
    {
        public uint[] floors;
        public byte elevator;
        public ushort steps;
        public State(uint[] f, byte e, ushort s)
        {
            floors = f;
            elevator = e;
            steps = s;
        }
    }
    struct Pair : IComparable<Pair>
    {
        public byte g, c;
        public int CompareTo(Pair other)
        {
            int d = g - other.g;
            return d != 0 ? d : c - other.c;
        }
    }
    static int GetMatId(string name, List<string> mats)
    {
        int idx = mats.IndexOf(name);
        if (idx >= 0) return idx;
        mats.Add(name);
        return mats.Count - 1;
    }
    static ulong GetKey(State s, int numMats)
    {
        Pair[] pairs = new Pair[numMats];
        for (int i = 0; i < numMats; i++)
        {
            for (int f = 0; f < NUM_FLOORS; f++)
            {
                if (((s.floors[f] >> i) & 1) != 0) pairs[i].g = (byte)f;
                if (((s.floors[f] >> (i + MAX_MATERIALS)) & 1) != 0) pairs[i].c = (byte)f;
            }
        }
        Array.Sort(pairs);
        ulong key = s.elevator;
        for (int i = 0; i < numMats; i++)
        {
            key = (key << 4) | (ulong)((pairs[i].g << 2) | pairs[i].c);
        }
        return key;
    }
    static bool IsFloorValid(uint floor)
    {
        uint gens = floor & ((1u << MAX_MATERIALS) - 1);
        uint chips = floor >> MAX_MATERIALS;
        return gens == 0 || (chips & ~gens) == 0;
    }
    static int Solve()
    {
        var lines = File.ReadAllLines("input.txt");
        List<string> mats = new List<string>();
        uint[] initFloors = new uint[NUM_FLOORS];
        for (int f = 0; f < lines.Length && f < NUM_FLOORS; f++)
        {
            var line = lines[f];
            foreach (var ch in new[] { ',', '.', '-' }) line = line.Replace(ch, ' ');
            var words = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            for (int i = 0; i < words.Length; i++)
            {
                if (words[i] == "generator")
                {
                    int id = GetMatId(words[i - 1], mats);
                    initFloors[f] |= 1u << id;
                }
                else if (words[i] == "microchip")
                {
                    int id = GetMatId(words[i - 2], mats);
                    initFloors[f] |= 1u << (id + MAX_MATERIALS);
                }
            }
        }
        int elId = GetMatId("elerium", mats);
        initFloors[0] |= 1u << elId;
        initFloors[0] |= 1u << (elId + MAX_MATERIALS);
        int diId = GetMatId("dilithium", mats);
        initFloors[0] |= 1u << diId;
        initFloors[0] |= 1u << (diId + MAX_MATERIALS);
        int numMats = mats.Count;
        uint finalMask = 0;
        for (int i = 0; i < numMats; i++) finalMask |= (1u << i) | (1u << (i + MAX_MATERIALS));
        var queue = new State[2000000];
        int head = 0, tail = 0;
        queue[tail++] = new State((uint[])initFloors.Clone(), 0, 0);
        var visited = new HashSet<ulong>();
        visited.Add(GetKey(queue[0], numMats));
        while (head < tail)
        {
            var cur = queue[head++];
            if (cur.floors[NUM_FLOORS - 1] == finalMask) return cur.steps;
            var items = new List<int>();
            for (int i = 0; i < 2 * MAX_MATERIALS; i++)
                if (((cur.floors[cur.elevator] >> i) & 1) != 0) items.Add(i);
            foreach (int d in new[] { -1, 1 })
            {
                int ne = cur.elevator + d;
                if (ne < 0 || ne >= NUM_FLOORS) continue;
                for (int i = 0; i < items.Count; i++)
                {
                    uint move1 = 1u << items[i];
                    var nxt = new State((uint[])cur.floors.Clone(), (byte)ne, (ushort)(cur.steps + 1));
                    nxt.floors[cur.elevator] &= ~move1;
                    nxt.floors[ne] |= move1;
                    if (IsFloorValid(nxt.floors[ne]) && IsFloorValid(nxt.floors[cur.elevator]))
                    {
                        ulong key = GetKey(nxt, numMats);
                        if (visited.Add(key)) queue[tail++] = nxt;
                    }
                    for (int j = i + 1; j < items.Count; j++)
                    {
                        uint move2 = move1 | (1u << items[j]);
                        var nxt2 = new State((uint[])cur.floors.Clone(), (byte)ne, (ushort)(cur.steps + 1));
                        nxt2.floors[cur.elevator] &= ~move2;
                        nxt2.floors[ne] |= move2;
                        if (IsFloorValid(nxt2.floors[ne]) && IsFloorValid(nxt2.floors[cur.elevator]))
                        {
                            ulong key = GetKey(nxt2, numMats);
                            if (visited.Add(key)) queue[tail++] = nxt2;
                        }
                    }
                }
            }
        }
        return -1;
    }
    static void Main()
    {
        Console.WriteLine(Solve());
    }
}