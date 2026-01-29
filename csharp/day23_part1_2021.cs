
using System;
using System.Collections.Generic;
using System.Text;

class Program
{
    const int ROWS = 5;
    const int COLS = 13;
    static readonly int[] ROOM_COLS = new int[] { 3, 5, 7, 9 };
    static readonly char[] TARGET_CHARS = new char[] { 'A', 'B', 'C', 'D' };

    static int EnergyCost(char t)
    {
        switch (t)
        {
            case 'A': return 1;
            case 'B': return 10;
            case 'C': return 100;
            case 'D': return 1000;
            default: return 0;
        }
    }

    static bool AllDone(char[,] grid)
    {
        for (int i = 0; i < ROOM_COLS.Length; i++)
        {
            int col = ROOM_COLS[i];
            char target = TARGET_CHARS[i];
            for (int r = 2; r < ROWS - 1; r++)
            {
                if (grid[r, col] != target) return false;
            }
        }
        return true;
    }

    static char GetTargetCharForCol(int col)
    {
        for (int i = 0; i < ROOM_COLS.Length; i++)
            if (ROOM_COLS[i] == col) return TARGET_CHARS[i];
        return '\0';
    }

    static bool IsTargetRoom(int r, int c, char amphipodType)
    {
        if (r >= 2 && r <= ROWS - 2)
        {
            return GetTargetCharForCol(c) == amphipodType;
        }
        return false;
    }

    static bool IsAmphipod(char ch) => ch >= 'A' && ch <= 'D';

    static string GridToString(char[,] grid)
    {
        var sb = new StringBuilder(ROWS * COLS);
        for (int r = 0; r < ROWS; r++)
            for (int c = 0; c < COLS; c++)
                sb.Append(grid[r, c]);
        return sb.ToString();
    }

    static char[,] CopyGrid(char[,] grid)
    {
        var g = new char[ROWS, COLS];
        for (int r = 0; r < ROWS; r++)
            for (int c = 0; c < COLS; c++)
                g[r, c] = grid[r, c];
        return g;
    }

    struct QueueNode
    {
        public int R;
        public int C;
        public int Dist;
    }

    struct Move
    {
        public int R;
        public int C;
        public int Energy;
    }

    class State
    {
        public char[,] Grid;
        public int Energy;
        public State(char[,] g, int e) { Grid = g; Energy = e; }
    }

    class MinHeap
    {
        private List<State> data = new List<State>();
        public int Count => data.Count;

        public void Push(State s)
        {
            data.Add(s);
            SiftUp(data.Count - 1);
        }

        public State Pop()
        {
            if (data.Count == 0) throw new InvalidOperationException();
            var min = data[0];
            var last = data[data.Count - 1];
            data[0] = last;
            data.RemoveAt(data.Count - 1);
            SiftDown(0);
            return min;
        }

        private void SiftUp(int i)
        {
            while (i > 0)
            {
                int p = (i - 1) / 2;
                if (data[i].Energy < data[p].Energy)
                {
                    var t = data[i]; data[i] = data[p]; data[p] = t;
                    i = p;
                }
                else break;
            }
        }

        private void SiftDown(int i)
        {
            int n = data.Count;
            while (true)
            {
                int l = 2 * i + 1;
                int r = l + 1;
                int smallest = i;
                if (l < n && data[l].Energy < data[smallest].Energy) smallest = l;
                if (r < n && data[r].Energy < data[smallest].Energy) smallest = r;
                if (smallest != i)
                {
                    var t = data[i]; data[i] = data[smallest]; data[smallest] = t;
                    i = smallest;
                }
                else break;
            }
        }
    }

    static List<Move> GetNextMoves(char[,] grid, int startR, int startC, char amphipodType)
    {
        var moves = new List<Move>();
        int energyMult = EnergyCost(amphipodType);
        bool startedInHallway = (startR == 1);

        bool[,] visited = new bool[ROWS, COLS];
        var queue = new List<QueueNode>();
        int head = 0;

        queue.Add(new QueueNode { R = startR, C = startC, Dist = 0 });
        visited[startR, startC] = true;

        int[] dr = { -1, 1, 0, 0 };
        int[] dc = { 0, 0, -1, 1 };

        while (head < queue.Count)
        {
            var cur = queue[head++];
            int r = cur.R;
            int c = cur.C;
            int dist = cur.Dist;

            if (dist > 0)
            {
                if (startedInHallway)
                {
                    if (IsTargetRoom(r, c, amphipodType))
                    {
                        bool roomReady = true;
                        for (int rr = r + 1; rr < ROWS - 1; rr++)
                        {
                            char ch = grid[rr, c];
                            if (ch == '.') { roomReady = false; break; }
                            if (ch != amphipodType) { roomReady = false; break; }
                        }
                        bool deeperAvailable = false;
                        for (int rr = r + 1; rr < ROWS - 1; rr++)
                        {
                            if (grid[rr, c] == '.') { deeperAvailable = true; break; }
                        }
                        if (roomReady && !deeperAvailable)
                        {
                            moves.Add(new Move { R = r, C = c, Energy = dist * energyMult });
                        }
                    }
                }
                else
                {
                    if (r == 1)
                    {
                        bool isEntrance = false;
                        for (int i = 0; i < ROOM_COLS.Length; i++) if (c == ROOM_COLS[i]) { isEntrance = true; break; }
                        if (!isEntrance)
                        {
                            moves.Add(new Move { R = r, C = c, Energy = dist * energyMult });
                        }
                    }
                }
            }

            for (int k = 0; k < 4; k++)
            {
                int nr = r + dr[k];
                int nc = c + dc[k];
                if (nr >= 0 && nr < ROWS && nc >= 0 && nc < COLS && grid[nr, nc] == '.' && !visited[nr, nc])
                {
                    visited[nr, nc] = true;
                    queue.Add(new QueueNode { R = nr, C = nc, Dist = dist + 1 });
                }
            }
        }

        return moves;
    }

    static void Solve()
    {
        char[,] grid = new char[ROWS, COLS];
        using (var sr = new System.IO.StreamReader("input.txt"))
        {
            for (int r = 0; r < ROWS; r++)
            {
                string line = sr.ReadLine() ?? "";
                for (int c = 0; c < COLS; c++)
                    grid[r, c] = c < line.Length ? line[c] : ' ';
            }
        }

        var heap = new MinHeap();
        var seen = new Dictionary<string, int>();

        var initial = new State(grid, 0);
        string initKey = GridToString(grid);
        seen[initKey] = 0;
        heap.Push(initial);

        int answer = -1;

        while (heap.Count > 0)
        {
            var cur = heap.Pop();
            if (AllDone(cur.Grid))
            {
                answer = cur.Energy;
                break;
            }

            for (int r = 1; r < ROWS - 1; r++)
            {
                for (int c = 1; c < COLS - 1; c++)
                {
                    char amph = cur.Grid[r, c];
                    if (!IsAmphipod(amph)) continue;

                    bool inHallway = (r == 1);
                    bool inCorrect = IsTargetRoom(r, c, amph);
                    bool needsMoving = false;
                    if (inHallway) needsMoving = true;
                    else if (!inCorrect) needsMoving = true;
                    else
                    {
                        bool blocking = false;
                        for (int rr = r + 1; rr < ROWS - 1; rr++)
                            if (cur.Grid[rr, c] != amph) { blocking = true; break; }
                        if (blocking) needsMoving = true;
                    }
                    if (!needsMoving) continue;

                    var moves = GetNextMoves(cur.Grid, r, c, amph);
                    foreach (var mv in moves)
                    {
                        var nextGrid = CopyGrid(cur.Grid);
                        nextGrid[r, c] = '.';
                        nextGrid[mv.R, mv.C] = amph;
                        int nextEnergy = cur.Energy + mv.Energy;

                        var nextState = new State(nextGrid, nextEnergy);
                        string key = GridToString(nextGrid);
                        if (!seen.TryGetValue(key, out int old) || nextEnergy < old)
                        {
                            seen[key] = nextEnergy;
                            heap.Push(nextState);
                        }
                    }
                }
            }
        }

        Console.WriteLine(answer);
    }

    static void Main()
    {
        Solve();
    }

    // Helper types for internal usage
}
