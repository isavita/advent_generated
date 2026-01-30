// part1_day25_2019.cs
// AoC 2019 Day 25 (Intcode text adventure) automated solver
// Compatible with older Mono mcs (C# 6-ish): no tuples, no switch-expressions, no local functions.
//
// Compile:
//   mcs -optimize part1_day25_2019.cs
// Run:
//   mono part1_day25_2019.exe input.txt

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

public static class Program
{
    // ----------------------------- Intcode Emulator -----------------------------

    private sealed class Emulator
    {
        private readonly Dictionary<long, long> _mem;
        private readonly Queue<long> _input;
        private long _ip;
        private long _rb;

        public Emulator(List<long> program)
        {
            _mem = new Dictionary<long, long>(program.Count * 2 + 16);
            for (int i = 0; i < program.Count; i++) _mem[(long)i] = program[i];
            _input = new Queue<long>();
            _ip = 0;
            _rb = 0;
        }

        public enum Status
        {
            Halted,
            Output,
            WaitingForInput
        }

        public struct StepResult
        {
            public Status Status;
            public long Output;     // valid iff Status == Output
        }

        private long MemGet(long addr)
        {
            long v;
            return _mem.TryGetValue(addr, out v) ? v : 0L;
        }

        private void MemSet(long addr, long v)
        {
            _mem[addr] = v;
        }

        public void WriteString(string s)
        {
            for (int i = 0; i < s.Length; i++)
                _input.Enqueue((long)s[i]);
        }

        private static long Pow10(int n)
        {
            long p = 1;
            for (int i = 0; i < n; i++) p *= 10;
            return p;
        }

        private long Mode(long instruction, int offset)
        {
            // offset is 1-based for params in this code
            return (instruction / Pow10(offset + 1)) % 10;
        }

        private long GetParameter(long instruction, int offset)
        {
            long mode = Mode(instruction, offset);
            long param = MemGet(_ip + offset);

            if (mode == 0) return MemGet(param);
            if (mode == 1) return param;
            if (mode == 2) return MemGet(_rb + param);

            throw new Exception("Unknown parameter mode " + mode + " at ip " + _ip);
        }

        private long GetWriteAddress(long instruction, int offset)
        {
            long mode = Mode(instruction, offset);
            long param = MemGet(_ip + offset);

            if (mode == 0) return param;
            if (mode == 2) return _rb + param;

            throw new Exception("Invalid mode for writing " + mode + " at ip " + _ip);
        }

        public StepResult Emulate()
        {
            while (true)
            {
                long instruction = MemGet(_ip);
                long opcode = instruction % 100;

                if (opcode == 1)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    long c = GetWriteAddress(instruction, 3);
                    MemSet(c, a + b);
                    _ip += 4;
                }
                else if (opcode == 2)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    long c = GetWriteAddress(instruction, 3);
                    MemSet(c, a * b);
                    _ip += 4;
                }
                else if (opcode == 3)
                {
                    if (_input.Count == 0)
                        return new StepResult { Status = Status.WaitingForInput, Output = 0 };

                    long addr = GetWriteAddress(instruction, 1);
                    MemSet(addr, _input.Dequeue());
                    _ip += 2;
                }
                else if (opcode == 4)
                {
                    long a = GetParameter(instruction, 1);
                    _ip += 2;
                    return new StepResult { Status = Status.Output, Output = a };
                }
                else if (opcode == 5)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    _ip = (a != 0) ? b : (_ip + 3);
                }
                else if (opcode == 6)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    _ip = (a == 0) ? b : (_ip + 3);
                }
                else if (opcode == 7)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    long c = GetWriteAddress(instruction, 3);
                    MemSet(c, (a < b) ? 1 : 0);
                    _ip += 4;
                }
                else if (opcode == 8)
                {
                    long a = GetParameter(instruction, 1);
                    long b = GetParameter(instruction, 2);
                    long c = GetWriteAddress(instruction, 3);
                    MemSet(c, (a == b) ? 1 : 0);
                    _ip += 4;
                }
                else if (opcode == 9)
                {
                    long a = GetParameter(instruction, 1);
                    _rb += a;
                    _ip += 2;
                }
                else if (opcode == 99)
                {
                    return new StepResult { Status = Status.Halted, Output = 0 };
                }
                else
                {
                    throw new Exception("Unknown opcode " + opcode + " at ip " + _ip);
                }
            }
        }
    }

    // ----------------------------- World / State -----------------------------

    private sealed class Room
    {
        public readonly string Name;
        // value is destination room name, or null if unexplored
        public readonly Dictionary<string, string> Connections = new Dictionary<string, string>();

        public Room(string name) { Name = name; }
    }

    private enum Mode
    {
        Explore,
        Navigate,
        Test
    }

    private static readonly Dictionary<string, string> Opposite = new Dictionary<string, string>
    {
        { "north", "south" },
        { "south", "north" },
        { "west",  "east"  },
        { "east",  "west"  },
    };

    private static readonly HashSet<string> Blacklist = new HashSet<string>
    {
        "photons",
        "escape pod",
        "molten lava",
        "infinite loop",
        "giant electromagnet"
    };

    private sealed class State
    {
        public Emulator Emu;
        public Dictionary<string, Room> World = new Dictionary<string, Room>();
        public Dictionary<string, bool> Inventory = new Dictionary<string, bool>();

        public Mode Mode = Mode.Explore;
        public string CurrentRoom = null;

        // In Explore: stack (head = most recent previous room)
        // In Navigate: queue (head = next target room)
        public List<string> Path = new List<string>();

        public string Checkpoint = null;
        public string Floor = null;
        public string TestDir = "";

        public List<string> AvailableItems = new List<string>();
        public int ItemMask = 0;

        public string LastRoom = null;
        public List<string> LastItems = new List<string>();
        public string LastDir = "";

        public StringBuilder Output = new StringBuilder(4096);

        public State(Emulator emu) { Emu = emu; }
    }

    private static Room GetRoom(State st, string name)
    {
        Room r;
        if (!st.World.TryGetValue(name, out r))
        {
            r = new Room(name);
            st.World[name] = r;
        }
        return r;
    }

    // ----------------------------- Output Parsing -----------------------------

    private static void ProcessOutput(State st, string output, out List<string> items)
    {
        items = new List<string>();
        string[] lines = output.Split('\n');

        for (int i = 0; i < lines.Length; i++)
        {
            string line = lines[i].Trim();
            if (line.Length == 0 || line == "Command?") continue;

            if (line.StartsWith("== ") && line.EndsWith(" =="))
            {
                string name = line.Substring(3, line.Length - 6);
                GetRoom(st, name);
                st.CurrentRoom = name;
                items.Clear();

                // Skip description until blank
                i++;
                while (i < lines.Length && lines[i].Trim().Length != 0) i++;
                continue;
            }

            if (line == "Doors here lead:")
            {
                i++;
                while (i < lines.Length && lines[i].Trim().Length != 0)
                {
                    string doorLine = lines[i].Trim();
                    if (doorLine.StartsWith("- ") && st.CurrentRoom != null)
                    {
                        string dir = doorLine.Substring(2);
                        Room r = GetRoom(st, st.CurrentRoom);
                        if (!r.Connections.ContainsKey(dir)) r.Connections[dir] = null;
                    }
                    i++;
                }
                continue;
            }

            if (line == "Items here:")
            {
                i++;
                while (i < lines.Length && lines[i].Trim().Length != 0)
                {
                    string itemLine = lines[i].Trim();
                    if (itemLine.StartsWith("- "))
                    {
                        items.Add(itemLine.Substring(2));
                    }
                    i++;
                }
                continue;
            }

            if (line.StartsWith("You take the ") && line.EndsWith("."))
            {
                string taken = line.Substring("You take the ".Length,
                    line.Length - "You take the ".Length - 1);

                st.Inventory[taken] = true;

                if (st.LastRoom != null)
                {
                    st.CurrentRoom = st.LastRoom;
                    var filtered = new List<string>();
                    for (int k = 0; k < st.LastItems.Count; k++)
                    {
                        if (st.LastItems[k] != taken) filtered.Add(st.LastItems[k]);
                    }
                    items = filtered;
                }
                continue;
            }

            if (line.StartsWith("You drop the ") && line.EndsWith("."))
            {
                string dropped = line.Substring("You drop the ".Length,
                    line.Length - "You drop the ".Length - 1);

                st.Inventory[dropped] = false;

                if (st.LastRoom != null)
                {
                    st.CurrentRoom = st.LastRoom;
                    var tmp = new List<string>(st.LastItems);
                    tmp.Add(dropped);
                    items = tmp;
                }
                continue;
            }

            if (line.StartsWith("A loud, robotic voice says \"Alert!"))
            {
                HandleAlert(st);
                continue;
            }
        }
    }

    private static void HandleAlert(State st)
    {
        if (st.Mode == Mode.Explore)
        {
            if (st.Path.Count > 0) st.Path.RemoveAt(0); // pop head

            st.Checkpoint = st.LastRoom;
            st.Floor = st.CurrentRoom;
            st.TestDir = st.LastDir;

            if (st.Checkpoint != null && st.Floor != null && st.TestDir.Length != 0)
            {
                Room cp = GetRoom(st, st.Checkpoint);
                cp.Connections[st.TestDir] = st.Floor;
            }
        }

        st.LastRoom = null;
        st.LastItems = new List<string>();
        st.LastDir = "";
    }

    private static void UpdateConnections(State st)
    {
        if (st.LastRoom == null || st.CurrentRoom == null || st.LastDir.Length == 0) return;

        Room last = GetRoom(st, st.LastRoom);
        Room cur = GetRoom(st, st.CurrentRoom);

        string dest;
        bool had = last.Connections.TryGetValue(st.LastDir, out dest);
        if (!had || dest == null)
        {
            last.Connections[st.LastDir] = st.CurrentRoom;
            cur.Connections[Opposite[st.LastDir]] = st.LastRoom;
        }
    }

    private static string ExtractCode(string output)
    {
        string[] lines = output.Split('\n');
        const string prefix = "\"Oh, hello! You should be able to get in by typing ";

        for (int i = 0; i < lines.Length; i++)
        {
            string line = lines[i].Trim();
            if (!line.StartsWith(prefix)) continue;

            int start = prefix.Length;
            int j = start;
            while (j < line.Length && char.IsDigit(line[j])) j++;
            if (j > start) return line.Substring(start, j - start);
        }
        return null;
    }

    // ----------------------------- BFS / Direction Helpers -----------------------------

    private sealed class NodePath
    {
        public string Node;
        public List<string> Path;
        public NodePath(string node, List<string> path) { Node = node; Path = path; }
    }

    private static List<string> FindPath(State st, string from, string to)
    {
        var q = new Queue<NodePath>();
        q.Enqueue(new NodePath(from, new List<string> { from }));
        var visited = new HashSet<string>();
        visited.Add(from);

        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            if (cur.Node == to) return cur.Path;

            Room r = GetRoom(st, cur.Node);
            foreach (var kv in r.Connections)
            {
                string next = kv.Value;
                if (next != null && visited.Add(next))
                {
                    var p2 = new List<string>(cur.Path);
                    p2.Add(next);
                    q.Enqueue(new NodePath(next, p2));
                }
            }
        }

        return null;
    }

    private static string DirectionTo(State st, string from, string to)
    {
        Room r = GetRoom(st, from);
        foreach (var kv in r.Connections)
        {
            if (kv.Value == to) return kv.Key;
        }
        return null;
    }

    private static string ChooseUnexplored(State st, string room)
    {
        Room r = GetRoom(st, room);

        string[] preferred = { "north", "south", "west", "east" };
        for (int i = 0; i < preferred.Length; i++)
        {
            string dest;
            if (r.Connections.TryGetValue(preferred[i], out dest) && dest == null)
                return preferred[i];
        }

        foreach (var kv in r.Connections)
        {
            if (kv.Value == null) return kv.Key;
        }
        return null;
    }

    // ----------------------------- Decision Logic -----------------------------

    private static void SendCommand(State st, string cmd)
    {
        st.Emu.WriteString(cmd);
    }

    private static void BootstrapMove(State st)
    {
        // If we got WaitingForInput before parsing any room header, move once to force state.
        string dir = "north";
        st.LastDir = dir;
        SendCommand(st, dir + "\n");
    }

    private static void Explore(State st, List<string> items)
    {
        // take first safe item
        for (int i = 0; i < items.Count; i++)
        {
            string it = items[i];
            if (!Blacklist.Contains(it))
            {
                SendCommand(st, "take " + it + "\n");
                return;
            }
        }

        if (st.CurrentRoom == null) { BootstrapMove(st); return; }

        string dir = ChooseUnexplored(st, st.CurrentRoom);
        if (dir != null)
        {
            st.Path.Insert(0, st.CurrentRoom); // push head
            st.LastDir = dir;
            SendCommand(st, dir + "\n");
            return;
        }

        // backtrack
        if (st.Path.Count > 0)
        {
            string prev = st.Path[0];
            st.Path.RemoveAt(0);

            string back = DirectionTo(st, st.CurrentRoom, prev);
            if (back == null) throw new Exception("Cannot go from \"" + st.CurrentRoom + "\" to \"" + prev + "\"");

            st.LastDir = back;
            SendCommand(st, back + "\n");
            return;
        }

        // exploration complete -> navigate to checkpoint
        if (st.Checkpoint != null && st.Floor != null)
        {
            List<string> p = FindPath(st, st.CurrentRoom, st.Checkpoint);
            if (p == null) throw new Exception("No path to checkpoint");

            st.Path = new List<string>();
            for (int i = 1; i < p.Count; i++) st.Path.Add(p[i]); // drop current
            st.Mode = Mode.Navigate;
            return;
        }

        throw new Exception("No checkpoint found");
    }

    private static void Navigate(State st)
    {
        if (st.CurrentRoom == null) { BootstrapMove(st); return; }

        if (st.Path.Count == 0)
        {
            var avail = new List<string>();
            foreach (var kv in st.Inventory)
                if (kv.Value) avail.Add(kv.Key);

            avail.Sort(StringComparer.Ordinal);
            st.AvailableItems = avail;
            st.ItemMask = 0;
            st.Mode = Mode.Test;
            return;
        }

        string next = st.Path[0];
        st.Path.RemoveAt(0);

        string dir = DirectionTo(st, st.CurrentRoom, next);
        if (dir == null) throw new Exception("Cannot go from \"" + st.CurrentRoom + "\" to \"" + next + "\"");

        st.LastDir = dir;
        SendCommand(st, dir + "\n");
    }

    private static void Test(State st)
    {
        var items = st.AvailableItems;
        int mask = st.ItemMask;
        int maxMask = 1 << items.Count;

        if (mask >= maxMask)
            throw new Exception("No valid item combination found after " + maxMask + " attempts");

        // bring inventory to target mask
        for (int i = 0; i < items.Count; i++)
        {
            string item = items[i];
            bool target = (mask & (1 << i)) != 0;

            bool cur;
            st.Inventory.TryGetValue(item, out cur);

            if (cur != target)
            {
                string action = target ? "take " : "drop ";
                SendCommand(st, action + item + "\n");
                return;
            }
        }

        // try door
        st.ItemMask = mask + 1;

        if (st.TestDir.Length == 0) { BootstrapMove(st); return; }
        SendCommand(st, st.TestDir + "\n");
    }

    // ----------------------------- Entry Point -----------------------------

    public static void Main(string[] args)
    {
        string file = (args.Length > 0) ? args[0] : "input.txt";
        string text = File.ReadAllText(file).Trim();

        var parts = text.Split(',');
        var program = new List<long>(parts.Length);
        for (int i = 0; i < parts.Length; i++) program.Add(long.Parse(parts[i].Trim()));

        var st = new State(new Emulator(program));

        const int MaxActions = 30000;
        int actions = 0;

        while (true)
        {
            Emulator.StepResult res = st.Emu.Emulate();

            if (res.Status == Emulator.Status.Halted)
            {
                string outStr = st.Output.ToString();
                string code = ExtractCode(outStr);
                if (code != null)
                {
                    Console.WriteLine(code);
                    return;
                }

                Console.Error.WriteLine("Error: No solution found");
                Console.Error.WriteLine(outStr);
                return;
            }

            if (res.Status == Emulator.Status.Output)
            {
                st.Output.Append((char)res.Output);
                continue;
            }

            // Waiting for input
            if (actions++ > MaxActions)
                throw new Exception("Loop detected (action cap reached)");

            string output = st.Output.ToString();
            st.Output.Length = 0;

            List<string> items;
            ProcessOutput(st, output, out items);
            UpdateConnections(st);

            st.LastRoom = st.CurrentRoom;
            st.LastItems = items;
            st.LastDir = ""; // set by movement decisions

            if (st.Mode == Mode.Explore) Explore(st, items);
            else if (st.Mode == Mode.Navigate) Navigate(st);
            else Test(st);
        }
    }
}
