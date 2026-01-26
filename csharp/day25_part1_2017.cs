
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct Rule
    {
        public int Write, Move;
        public char Next;
        public Rule(int w, int m, char n) { Write = w; Move = m; Next = n; }
    }

    class State
    {
        public Rule[] Rules = new Rule[2];
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        char start = ' ';
        int steps = 0;
        var states = new Dictionary<char, State>();
        for (int i = 0; i < lines.Length; i++)
        {
            var line = lines[i].Trim();
            if (line.StartsWith("Begin in state"))
                start = line[line.Length - 2];
            else if (line.StartsWith("Perform a diagnostic checksum after"))
                steps = int.Parse(new string(line.Where(char.IsDigit).ToArray()));
            else if (line.StartsWith("In state"))
            {
                var s = line[line.Length - 2];
                var state = new State();
                // value 0
                i += 2; // skip "If the current value is 0:"
                var write0 = int.Parse(lines[i].Trim().Split(' ').Last().Trim('.'));
                var move0 = lines[++i].Trim().Contains("right") ? 1 : -1;
                var next0 = lines[++i].Trim()[lines[i].Trim().Length - 2];
                state.Rules[0] = new Rule(write0, move0, next0);
                // value 1
                i += 2; // skip "If the current value is 1:"
                var write1 = int.Parse(lines[i].Trim().Split(' ').Last().Trim('.'));
                var move1 = lines[++i].Trim().Contains("right") ? 1 : -1;
                var next1 = lines[++i].Trim()[lines[i].Trim().Length - 2];
                state.Rules[1] = new Rule(write1, move1, next1);
                states[s] = state;
            }
        }

        var tape = new Dictionary<int, int>();
        int cursor = 0;
        char curState = start;
        for (int i = 0; i < steps; i++)
        {
            int curVal = tape.TryGetValue(cursor, out var v) ? v : 0;
            var rule = states[curState].Rules[curVal];
            tape[cursor] = rule.Write;
            cursor += rule.Move;
            curState = rule.Next;
        }

        int checksum = 0;
        foreach (var v in tape.Values) checksum += v;
        Console.WriteLine(checksum);
    }
}
