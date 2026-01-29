using System;
using System.IO;
using System.Text;
using System.Collections.Generic;

class Program
{
    const int ROWS = 7;
    const int COLS = 13;

    struct Coord { public int r, c; public Coord(int r, int c){ this.r = r; this.c = c; } }

    class State
    {
        public char[,] Grid = new char[ROWS, COLS];
        public long EnergyUsed;
        public State Clone()
        {
            var g = new char[ROWS, COLS];
            for (int i = 0; i < ROWS; i++) for (int j = 0; j < COLS; j++) g[i, j] = Grid[i, j];
            return new State { Grid = g, EnergyUsed = this.EnergyUsed };
        }
        public string Key()
        {
            var sb = new StringBuilder(ROWS * COLS);
            for (int r = 0; r < ROWS; r++)
                for (int c = 0; c < COLS; c++) sb.Append(Grid[r, c]);
            return sb.ToString();
        }
    }

    // Min-Heap for States by EnergyUsed
    class MinHeap
    {
        List<State> a = new List<State>();
        public int Count => a.Count;
        void Swap(int i, int j)
        {
            var t = a[i]; a[i] = a[j]; a[j] = t;
        }
        public void Push(State s)
        {
            a.Add(s);
            int i = a.Count - 1;
            while (i > 0)
            {
                int p = (i - 1) / 2;
                if (a[i].EnergyUsed < a[p].EnergyUsed)
                {
                    Swap(i, p);
                    i = p;
                }
                else break;
            }
        }
        public State Pop()
        {
            var top = a[0];
            var last = a[a.Count - 1];
            a.RemoveAt(a.Count - 1);
            if (a.Count > 0)
            {
                a[0] = last;
                int i = 0;
                while (true)
                {
                    int l = 2 * i + 1;
                    int r = 2 * i + 2;
                    int smallest = i;
                    if (l < a.Count && a[l].EnergyUsed < a[smallest].EnergyUsed) smallest = l;
                    if (r < a.Count && a[r].EnergyUsed < a[smallest].EnergyUsed) smallest = r;
                    if (smallest != i)
                    {
                        Swap(i, smallest);
                        i = smallest;
                    }
                    else break;
                }
            }
            return top;
        }
        public bool Any() => Count > 0;
    }

    static char RoomCoordToWantChar(int r, int c)
    {
        if (r >= 2 && r <= 5)
        {
            if (c == 3) return 'A';
            if (c == 5) return 'B';
            if (c == 7) return 'C';
            if (c == 9) return 'D';
        }
        return '.';
    }

    static bool IsAllDone(State s)
    {
        for (int r = 2; r <= 5; r++)
        {
            for (int c = 3; c <= 9; c += 2)
            {
                if (s.Grid[r, c] != RoomCoordToWantChar(r, c))
                    return false;
            }
        }
        return true;
    }

    static int GetUnsettledCoords(State s, List<Coord> unsettled)
    {
        unsettled.Clear();
        // Hallway
        for (int c = 1; c < COLS - 1; c++)
        {
            char ch = s.Grid[1, c];
            if (ch >= 'A' && ch <= 'D')
                unsettled.Add(new Coord(1, c));
        }
        // Rooms
        for (int c = 3; c <= 9; c += 2)
        {
            bool roomOkBelow = true;
            for (int r = 5; r >= 2; --r)
            {
                char currentChar = s.Grid[r, c];
                char wantChar = RoomCoordToWantChar(r, c);
                if (currentChar == '.')
                {
                    roomOkBelow = false;
                }
                else if (currentChar != wantChar)
                {
                    unsettled.Add(new Coord(r, c));
                    roomOkBelow = false;
                }
                else
                {
                    if (!roomOkBelow)
                        unsettled.Add(new Coord(r, c));
                }
            }
        }
        return unsettled.Count;
    }

    static long CalcEnergy(char type, Coord start, Coord end)
    {
        long baseEnergy;
        switch (type)
        {
            case 'A': baseEnergy = 1; break;
            case 'B': baseEnergy = 10; break;
            case 'C': baseEnergy = 100; break;
            case 'D': baseEnergy = 1000; break;
            default: return -1;
        }
        int dist;
        if (start.r > 1 && end.r == 1)
            dist = (start.r - 1) + Math.Abs(start.c - end.c);
        else if (start.r == 1 && end.r > 1)
            dist = (end.r - 1) + Math.Abs(start.c - end.c);
        else if (start.r > 1 && end.r > 1)
            dist = (start.r - 1) + (end.r - 1) + Math.Abs(start.c - end.c);
        else
            dist = Math.Abs(start.r - end.r) + Math.Abs(start.c - end.c);
        return baseEnergy * dist;
    }

    static void FindAndProcessMoves(State currentState, Coord startCoord, List<Coord> unsettled, Dictionary<string, long> best, MinHeap heap)
    {
        char amphipodType = currentState.Grid[startCoord.r, startCoord.c];
        bool startedInHallway = (startCoord.r == 1);

        // BFS
        bool[,] visited = new bool[ROWS, COLS];
        Queue<(Coord pos, int dist)> q = new Queue<(Coord, int)>();
        q.Enqueue((startCoord, 0));
        visited[startCoord.r, startCoord.c] = true;

        int[] dr = new int[] { -1, 1, 0, 0 };
        int[] dc = new int[] { 0, 0, -1, 1 };

        while (q.Count > 0)
        {
            var (pos, dist) = q.Dequeue();
            if (!(pos.r == startCoord.r && pos.c == startCoord.c))
            {
                char targetWantChar = RoomCoordToWantChar(pos.r, pos.c);
                if (startedInHallway)
                {
                    if (targetWantChar == amphipodType)
                    {
                        bool roomValid = true;
                        for (int r = pos.r + 1; r <= 5; ++r)
                        {
                            if (currentState.Grid[r, pos.c] != amphipodType)
                            {
                                roomValid = false;
                                break;
                            }
                        }
                        if (roomValid)
                        {
                            State nextState = currentState.Clone();
                            nextState.Grid[pos.r, pos.c] = amphipodType;
                            nextState.Grid[startCoord.r, startCoord.c] = '.';
                            nextState.EnergyUsed += CalcEnergy(amphipodType, startCoord, pos);

                            string key = nextState.Key();
                            if (!best.TryGetValue(key, out long existing) || nextState.EnergyUsed < existing)
                            {
                                best[key] = nextState.EnergyUsed;
                                heap.Push(nextState);
                            }
                        }
                    }
                }
                else
                {
                    // From room
                    if (pos.r == 1)
                    {
                        if (pos.c != 3 && pos.c != 5 && pos.c != 7 && pos.c != 9)
                        {
                            State nextState = currentState.Clone();
                            nextState.Grid[pos.r, pos.c] = amphipodType;
                            nextState.Grid[startCoord.r, startCoord.c] = '.';
                            nextState.EnergyUsed += CalcEnergy(amphipodType, startCoord, pos);

                            string key = nextState.Key();
                            if (!best.TryGetValue(key, out long existing) || nextState.EnergyUsed < existing)
                            {
                                best[key] = nextState.EnergyUsed;
                                heap.Push(nextState);
                            }
                        }
                    }
                    else if (targetWantChar == amphipodType)
                    {
                        bool roomValid = true;
                        for (int r = pos.r + 1; r <= 5; ++r)
                        {
                            char ch = currentState.Grid[r, pos.c];
                            if (ch != amphipodType && ch != '.')
                            {
                                roomValid = false;
                                break;
                            }
                        }
                        for (int r = pos.r + 1; r <= 5; ++r)
                        {
                            if (currentState.Grid[r, pos.c] == '.')
                            {
                                roomValid = false;
                                break;
                            }
                        }
                        if (roomValid)
                        {
                            State nextState = currentState.Clone();
                            nextState.Grid[pos.r, pos.c] = amphipodType;
                            nextState.Grid[startCoord.r, startCoord.c] = '.';
                            nextState.EnergyUsed += CalcEnergy(amphipodType, startCoord, pos);

                            string key = nextState.Key();
                            if (!best.TryGetValue(key, out long existing) || nextState.EnergyUsed < existing)
                            {
                                best[key] = nextState.EnergyUsed;
                                heap.Push(nextState);
                            }
                        }
                    }
                }
            }

            for (int i = 0; i < 4; i++)
            {
                var nextPos = new Coord(pos.r + dr[i], pos.c + dc[i]);
                if (nextPos.r >= 0 && nextPos.r < ROWS && nextPos.c >= 0 && nextPos.c < COLS &&
                    currentState.Grid[nextPos.r, nextPos.c] == '.' && !visited[nextPos.r, nextPos.c])
                {
                    visited[nextPos.r, nextPos.c] = true;
                    q.Enqueue((nextPos, dist + 1));
                }
            }
        }
    }

    static void Main()
    {
        string inputPath = "input.txt";
        if (!File.Exists(inputPath))
        {
            Console.Error.WriteLine("Error opening input.txt");
            return;
        }

        State start = new State { EnergyUsed = 0 };
        // Initialize grid with walls
        for (int r = 0; r < ROWS; r++) for (int c = 0; c < COLS; c++) start.Grid[r, c] = '#';

        // Read 5 lines
        string[] lines = new string[5];
        using (var reader = new StreamReader(inputPath))
        {
            for (int i = 0; i < 5; i++)
            {
                string line = reader.ReadLine();
                if (line == null) line = "";
                lines[i] = line;
            }
        }

        // Fill first 5 rows from input
        for (int i = 0; i < 5; i++)
        {
            string s = lines[i];
            int len = s.Length;
            for (int j = 0; j < COLS; j++)
            {
                char ch = j < len ? s[j] : '#';
                start.Grid[i, j] = ch;
            }
        }

        // Shift existing rows and insert Part 2 rows
        // Original Row 3 -> New Row 5
        for (int j = 0; j < COLS; j++) start.Grid[5, j] = start.Grid[3, j];
        // Original Row 4 -> New Row 6
        for (int j = 0; j < COLS; j++) start.Grid[6, j] = start.Grid[4, j];

        // Insert new rows 3 and 4
        string row3 = "  #D#C#B#A#";
        for (int j = 0; j < COLS; j++) start.Grid[3, j] = (j < row3.Length) ? row3[j] : ' ';
        string row4 = "  #D#B#A#C#";
        for (int j = 0; j < COLS; j++) start.Grid[4, j] = (j < row4.Length) ? row4[j] : ' ';

        // Walls normalization
        for (int r = 0; r < ROWS; r++)
        {
            for (int c = 0; c < COLS; c++)
            {
                if (r == 0 || r == ROWS - 1 || c == 0 || c == COLS - 1 || (r > 1 && (c < 1 || c > 11)))
                    if (start.Grid[r, c] != '#') start.Grid[r, c] = '#';
                if (start.Grid[r, c] == ' ') start.Grid[r, c] = '#';
            }
        }

        // Hallway explicit '.'
        for (int c = 1; c <= 11; c++) start.Grid[1, c] = '.';
        // Rooms '.' initial
        for (int r = 2; r <= 5; r++)
            for (int c = 3; c <= 9; c += 2)
                if (start.Grid[r, c] != 'A' && start.Grid[r, c] != 'B' && start.Grid[r, c] != 'C' && start.Grid[r, c] != 'D')
                    start.Grid[r, c] = '.';

        // Initialize search
        var best = new Dictionary<string, long>();
        var heap = new MinHeap();

        string startKey = start.Key();
        best[startKey] = 0;
        heap.Push(start);

        List<Coord> unsettled = new List<Coord>(16);

        long finalEnergy = -1;

        while (heap.Count > 0)
        {
            State current = heap.Pop();

            if (IsAllDone(current))
            {
                finalEnergy = current.EnergyUsed;
                break;
            }

            GetUnsettledCoords(current, unsettled);
            foreach (var sc in unsettled)
            {
                FindAndProcessMoves(current, sc, unsettled, best, heap);
            }
        }

        if (finalEnergy != -1)
            Console.WriteLine(finalEnergy);
        else
            Console.WriteLine("No solution found");
    }
}