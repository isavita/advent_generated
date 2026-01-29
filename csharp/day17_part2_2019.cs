using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

class IntcodeComputer
{
    private readonly Dictionary<long, long> memory = new Dictionary<long, long>();
    private long ip = 0;
    private long rb = 0;
    public Queue<long> Inputs { get; } = new Queue<long>();
    public Queue<long> Outputs { get; } = new Queue<long>();
    public bool Halted { get; private set; } = false;
    public bool WaitingForInput { get; private set; } = false;

    public IntcodeComputer(long[] program)
    {
        for (int i = 0; i < program.Length; i++) memory[i] = program[i];
        ip = 0;
        rb = 0;
    }

    private long Mem(long a) => memory.TryGetValue(a, out var v) ? v : 0;

    private void SetMem(long a, long v) => memory[a] = v;

    private long GetParam(int mode, long paramAddr)
    {
        var raw = Mem(paramAddr);
        switch (mode)
        {
            case 0: return Mem(raw);
            case 1: return raw;
            case 2: return Mem(rb + raw);
            default: throw new InvalidOperationException("Unknown parameter mode");
        }
    }

    private void SetParam(int mode, long paramAddr, long value)
    {
        var raw = Mem(paramAddr);
        switch (mode)
        {
            case 0: SetMem(raw, value); break;
            case 2: SetMem(rb + raw, value); break;
            default: throw new InvalidOperationException("Unknown parameter mode for write");
        }
    }

    public void Reset(long[] program)
    {
        memory.Clear();
        for (int i = 0; i < program.Length; i++) memory[i] = program[i];
        ip = 0;
        rb = 0;
        Halted = false;
        WaitingForInput = false;
        while (Inputs.Count > 0) Inputs.Dequeue();
        while (Outputs.Count > 0) Outputs.Dequeue();
    }

    public void Run()
    {
        WaitingForInput = false;
        while (!Halted && !WaitingForInput)
        {
            long instr = Mem(ip);
            int op = (int)(instr % 100);
            int m1 = (int)((instr / 100) % 10);
            int m2 = (int)((instr / 1000) % 10);
            int m3 = (int)((instr / 10000) % 10);

            switch (op)
            {
                case 1:
                    {
                        var a = GetParam(m1, ip + 1);
                        var b = GetParam(m2, ip + 2);
                        SetParam(m3, ip + 3, a + b);
                        ip += 4;
                        break;
                    }
                case 2:
                    {
                        var a = GetParam(m1, ip + 1);
                        var b = GetParam(m2, ip + 2);
                        SetParam(m3, ip + 3, a * b);
                        ip += 4;
                        break;
                    }
                case 3:
                    {
                        if (Inputs.Count == 0)
                        {
                            WaitingForInput = true;
                            return;
                        }
                        var v = Inputs.Dequeue();
                        SetParam(m1, ip + 1, v);
                        ip += 2;
                        break;
                    }
                case 4:
                    {
                        var v = GetParam(m1, ip + 1);
                        Outputs.Enqueue(v);
                        ip += 2;
                        break;
                    }
                case 5:
                    {
                        var p1 = GetParam(m1, ip + 1);
                        var p2 = GetParam(m2, ip + 2);
                        if (p1 != 0) ip = p2;
                        else ip += 3;
                        break;
                    }
                case 6:
                    {
                        var p1 = GetParam(m1, ip + 1);
                        var p2 = GetParam(m2, ip + 2);
                        if (p1 == 0) ip = p2;
                        else ip += 3;
                        break;
                    }
                case 7:
                    {
                        var p1 = GetParam(m1, ip + 1);
                        var p2 = GetParam(m2, ip + 2);
                        SetParam(m3, ip + 3, p1 < p2 ? 1 : 0);
                        ip += 4;
                        break;
                    }
                case 8:
                    {
                        var p1 = GetParam(m1, ip + 1);
                        var p2 = GetParam(m2, ip + 2);
                        SetParam(m3, ip + 3, p1 == p2 ? 1 : 0);
                        ip += 4;
                        break;
                    }
                case 9:
                    {
                        var p = GetParam(m1, ip + 1);
                        rb += p;
                        ip += 2;
                        break;
                    }
                case 99:
                    Halted = true;
                    return;
                default:
                    throw new InvalidOperationException("Unknown opcode");
            }
        }
    }
}

class Program
{
    const int MAX_FUNC_LEN = 21;

    static bool TokensMatch(string[] tokens, int start, int patStart, int patLen)
    {
        if (start + patLen > tokens.Length) return false;
        for (int i = 0; i < patLen; i++)
        {
            if (tokens[start + i] != tokens[patStart + i]) return false;
        }
        return true;
    }

    static string JoinTokens(string[] tokens, int start, int len)
    {
        if (len <= 0) return "";
        return string.Join(",", tokens, start, len);
    }

    static bool CompressPath(string[] tokens, out string mainRoutine, out string A, out string B, out string C)
    {
        mainRoutine = A = B = C = "";
        for (int aLen = 1; aLen <= 10 && aLen <= tokens.Length; aLen++)
        {
            string a = JoinTokens(tokens, 0, aLen);
            if (a.Length > MAX_FUNC_LEN) continue;

            int bStart = 0;
            while (bStart < tokens.Length && TokensMatch(tokens, bStart, 0, aLen)) bStart += aLen;
            if (bStart >= tokens.Length) continue;

            for (int bLen = 1; bStart + bLen <= tokens.Length && bLen <= 10; bLen++)
            {
                string b = JoinTokens(tokens, bStart, bLen);
                if (b.Length > MAX_FUNC_LEN) continue;

                int cStart = 0;
                while (cStart < tokens.Length)
                {
                    if (TokensMatch(tokens, cStart, 0, aLen)) cStart += aLen;
                    else if (cStart + bLen <= tokens.Length && TokensMatch(tokens, cStart, bStart, bLen)) cStart += bLen;
                    else break;
                }
                if (cStart >= tokens.Length) continue;

                for (int cLen = 1; cStart + cLen <= tokens.Length && cLen <= 10; cLen++)
                {
                    string c = JoinTokens(tokens, cStart, cLen);
                    if (c.Length > MAX_FUNC_LEN) continue;

                    string main = "";
                    int pos = 0;
                    int mainLen = 0;
                    while (pos < tokens.Length)
                    {
                        bool matched = false;
                        if (TokensMatch(tokens, pos, 0, aLen))
                        {
                            if (mainLen > 0) main += ",";
                            main += "A";
                            pos += aLen;
                            mainLen++;
                            matched = true;
                        }
                        else if (pos + bLen <= tokens.Length && TokensMatch(tokens, pos, bStart, bLen))
                        {
                            if (mainLen > 0) main += ",";
                            main += "B";
                            pos += bLen;
                            mainLen++;
                            matched = true;
                        }
                        else if (pos + cLen <= tokens.Length && TokensMatch(tokens, pos, cStart, cLen))
                        {
                            if (mainLen > 0) main += ",";
                            main += "C";
                            pos += cLen;
                            mainLen++;
                            matched = true;
                        }
                        if (!matched) break;
                        if (main.Length > MAX_FUNC_LEN) break;
                    }
                    if (pos == tokens.Length && main.Length <= MAX_FUNC_LEN)
                    {
                        mainRoutine = main;
                        A = a;
                        B = b;
                        C = c;
                        return true;
                    }
                }
            }
        }
        return false;
    }

    static void Main(string[] args)
    {
        var programInput = File.ReadAllText("input.txt").Trim();
        var parts = programInput.Split(new[] { ',' }, StringSplitOptions.RemoveEmptyEntries);
        long[] programOriginal = parts.Select(p => long.Parse(p)).ToArray();

        // Part One
        var comp1 = new IntcodeComputer(programOriginal);
        comp1.Run();
        var mapLines = new List<string>();
        StringBuilder line = new StringBuilder();
        while (comp1.Outputs.Count > 0)
        {
            var v = comp1.Outputs.Dequeue();
            if (v == 10)
            {
                if (line.Length > 0)
                {
                    mapLines.Add(line.ToString());
                    line.Clear();
                }
                else
                {
                    mapLines.Add("");
                }
            }
            else
            {
                line.Append((char) v);
            }
        }
        if (line.Length > 0) mapLines.Add(line.ToString());

        int height = mapLines.Count;
        int width = mapLines.Any() ? mapLines.Max(s => s.Length) : 0;

        long alignmentSum = 0;
        for (int r = 1; r < height - 1; r++)
        {
            for (int c = 1; c < width - 1; c++)
            {
                bool self = r < mapLines.Count && c < mapLines[r].Length && mapLines[r][c] == '#';
                if (!self) continue;
                bool up = mapLines[r - 1].Length > c && mapLines[r - 1][c] == '#';
                bool down = mapLines[r + 1].Length > c && mapLines[r + 1][c] == '#';
                bool left = mapLines[r].Length > c - 1 && mapLines[r][c - 1] == '#';
                bool right = mapLines[r].Length > c + 1 && mapLines[r][c + 1] == '#';
                if (up && down && left && right)
                {
                    alignmentSum += r * c;
                }
            }
        }

        Console.WriteLine("Part One: " + alignmentSum);

        // Part Two
        long[] programPart2 = new long[programOriginal.Length];
        Array.Copy(programOriginal, programPart2, programOriginal.Length);
        programPart2[0] = 2;

        // Find robot and path
        int robotR = -1, robotC = -1;
        char robotDir = '?';
        for (int r = 0; r < height; r++)
        {
            for (int c = 0; c < (mapLines[r]?.Length ?? 0); c++)
            {
                char ch = mapLines[r][c];
                if ("^v<>".Contains(ch))
                {
                    robotR = r;
                    robotC = c;
                    robotDir = ch;
                    break;
                }
            }
            if (robotDir != '?') break;
        }
        if (robotDir == '?')
        {
            Console.WriteLine("Part Two: 0");
            return;
        }

        // Build tokens path
        string[] tokens;
        {
            int x = robotC, y = robotR;
            string dir = robotDir.ToString();
            List<string> tokList = new List<string>();

            int steps = 0;
            while (true)
            {
                int nx = x, ny = y;
                switch (dir)
                {
                    case "^": ny = y - 1; break;
                    case "v": ny = y + 1; break;
                    case "<": nx = x - 1; break;
                    case ">": nx = x + 1; break;
                }
                bool scaffoldAhead = ny >= 0 && ny < height && nx >= 0 && nx < (mapLines[ny]?.Length ?? 0) && mapLines[ny][nx] == '#';
                if (scaffoldAhead)
                {
                    steps++;
                    x = nx; y = ny;
                }
                else
                {
                    if (steps > 0)
                    {
                        tokList.Add(steps.ToString());
                        steps = 0;
                    }
                    char leftDir = Left(dir[0]);
                    int lx = x, ly = y;
                    switch (leftDir)
                    {
                        case '^': ly = y - 1; break;
                        case 'v': ly = y + 1; break;
                        case '<': lx = x - 1; break;
                        case '>': lx = x + 1; break;
                    }
                    bool leftOk = ly >= 0 && ly < height && lx >= 0 && lx < (mapLines[ly]?.Length ?? 0) && mapLines[ly][lx] == '#';
                    if (leftOk)
                    {
                        tokList.Add("L");
                        dir = leftDir.ToString();
                        continue;
                    }
                    char rightDir = Right(dir[0]);
                    int rx = x, ry = y;
                    switch (rightDir)
                    {
                        case '^': ry = y - 1; break;
                        case 'v': ry = y + 1; break;
                        case '<': rx = x - 1; break;
                        case '>': rx = x + 1; break;
                    }
                    bool rightOk = ry >= 0 && ry < height && rx >= 0 && rx < (mapLines[ry]?.Length ?? 0) && mapLines[ry][rx] == '#';
                    if (rightOk)
                    {
                        tokList.Add("R");
                        dir = rightDir.ToString();
                        continue;
                    }
                    break;
                }
            }
            if (steps > 0) tokList.Add(steps.ToString());
            tokens = tokList.ToArray();
        }

        if (!CompressPath(tokens, out string mainRoutine, out string A, out string B, out string C))
        {
            Console.WriteLine("Part Two: 0");
            return;
        }

        // Prepare inputs
        string[] inputs = new string[] { mainRoutine, A, B, C, "n" };
        var comp2 = new IntcodeComputer(programPart2);
        foreach (var s in inputs)
        {
            foreach (var ch in s)
            {
                comp2.Inputs.Enqueue((long)ch);
            }
            comp2.Inputs.Enqueue(10);
        }

        comp2.Run();
        long lastDust = 0;
        while (comp2.Outputs.Count > 0)
        {
            lastDust = comp2.Outputs.Dequeue();
        }

        Console.WriteLine("Part Two: " + lastDust);
    }

    static char Left(char d)
    {
        switch (d)
        {
            case '^': return '<';
            case '<': return 'v';
            case 'v': return '>';
            case '>': return '^';
            default: return '?';
        }
    }

    static char Right(char d)
    {
        switch (d)
        {
            case '^': return '>';
            case '>': return 'v';
            case 'v': return '<';
            case '<': return '^';
            default: return '?';
        }
    }
}