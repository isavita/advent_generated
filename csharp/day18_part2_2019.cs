using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int MAX_KEYS = 26;
    const int NUM_ROBOTS = 4;
    const int MAX_NODES = MAX_KEYS + NUM_ROBOTS;

    struct Point { public int X; public int Y; public Point(int x,int y){X=x;Y=y;} }

    struct Edge { public int To; public int Dist; public int ReqMask; public Edge(int t,int d,int r){To=t;Dist=d;ReqMask=r;} }

    struct BFSState { public int X; public int Y; public int Dist; public int ReqMask; public BFSState(int x,int y,int d,int r){X=x;Y=y;Dist=d;ReqMask=r;} }

    static int width = 0, height = 0;
    static char[,] grid;
    static Point[] nodeCoords = new Point[MAX_NODES];
    static List<Edge>[] keyGraph = new List<Edge>[MAX_NODES];
    static int numKeys = 0;
    static int allKeysMask = 0;

    static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        height = lines.Length;
        width = lines.Length > 0 ? lines[0].Length : 0;
        grid = new char[height, width];
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                grid[y, x] = lines[y][x];
            }
        }

        // Part Two Modification
        int centerX = -1, centerY = -1;
        bool found = false;
        for (int y = 1; y < height - 1 && !found; y++)
        {
            for (int x = 1; x < width - 1; x++)
            {
                if (grid[y, x] == '@')
                {
                    if (grid[y - 1, x] != '#' && grid[y + 1, x] != '#' &&
                        grid[y, x - 1] != '#' && grid[y, x + 1] != '#' &&
                        grid[y - 1, x - 1] != '#' && grid[y - 1, x + 1] != '#' &&
                        grid[y + 1, x - 1] != '#' && grid[y + 1, x + 1] != '#')
                    {
                        centerX = x; centerY = y; found = true; break;
                    }
                }
            }
        }
        if (!found)
        {
            Console.WriteLine("-1");
            return;
        }

        grid[centerY - 1, centerX - 1] = '@';
        grid[centerY - 1, centerX] = '#';
        grid[centerY - 1, centerX + 1] = '@';
        grid[centerY, centerX - 1] = '#';
        grid[centerY, centerX] = '#';
        grid[centerY, centerX + 1] = '#';
        grid[centerY + 1, centerX - 1] = '@';
        grid[centerY + 1, centerX] = '#';
        grid[centerY + 1, centerX + 1] = '@';

        // Find keys and robots
        numKeys = 0;
        int robotIdx = 0;
        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                char c = grid[y, x];
                if (c >= 'a' && c <= 'z')
                {
                    int keyId = c - 'a';
                    if (keyId >= MAX_KEYS) continue;
                    if (keyId >= numKeys) numKeys = keyId + 1;
                    nodeCoords[keyId] = new Point(x, y);
                }
                else if (c == '@')
                {
                    if (robotIdx < NUM_ROBOTS)
                    {
                        nodeCoords[numKeys + robotIdx] = new Point(x, y);
                        robotIdx++;
                    }
                }
            }
        }
        if (robotIdx != NUM_ROBOTS)
        {
            Console.WriteLine("-1");
            return;
        }

        int nodeCount = numKeys + NUM_ROBOTS;
        allKeysMask = (numKeys == 0) ? 0 : ((1 << numKeys) - 1);

        // Init graph
        for (int i = 0; i < nodeCount; i++) keyGraph[i] = new List<Edge>();

        // Build key graph
        for (int i = 0; i < nodeCount; i++) RunKeyBfs(i);

        // Dijkstra over states
        for (int i = 0; i < nodeCount; i++) keyGraph[i] = keyGraph[i]; // no-op, kept for clarity

        int result = Solve(nodeCount);
        Console.WriteLine(result);
    }

    static void RunKeyBfs(int startNodeId)
    {
        var start = nodeCoords[startNodeId];
        bool[,] visited = new bool[height, width];
        var q = new Queue<BFSState>();
        q.Enqueue(new BFSState(start.X, start.Y, 0, 0));
        visited[start.Y, start.X] = true;

        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            int x = cur.X, y = cur.Y, dist = cur.Dist, reqMask = cur.ReqMask;
            char cell = grid[y, x];

            if (cell >= 'a' && cell <= 'z')
            {
                int keyId = cell - 'a';
                if (keyId != startNodeId)
                {
                    keyGraph[startNodeId].Add(new Edge(keyId, dist, reqMask));
                }
            }

            int[] dx = { -1, 1, 0, 0 };
            int[] dy = { 0, 0, -1, 1 };
            for (int dir = 0; dir < 4; dir++)
            {
                int nx = x + dx[dir], ny = y + dy[dir];
                if (nx >= 0 && nx < width && ny >= 0 && ny < height && !visited[ny, nx])
                {
                    char nextCell = grid[ny, nx];
                    if (nextCell != '#')
                    {
                        visited[ny, nx] = true;
                        int nextMask = reqMask;
                        if (nextCell >= 'A' && nextCell <= 'Z')
                        {
                            int needed = Char.ToLower(nextCell) - 'a';
                            nextMask |= (1 << needed);
                        }
                        q.Enqueue(new BFSState(nx, ny, dist + 1, nextMask));
                    }
                }
            }
        }
    }

    static int Solve(int nodeCount)
    {
        var heap = new MinHeap();

        int[] initRobots = new int[NUM_ROBOTS];
        for (int i = 0; i < NUM_ROBOTS; i++) initRobots[i] = numKeys + i;

        var initState = new State(0, 0, initRobots);
        heap.Push(initState);

        var visited = new Dictionary<long, int>();
        long initKey = PackKey(initRobots, 0);
        visited[initKey] = 0;

        while (!heap.IsEmpty)
        {
            var cur = heap.Pop();
            long curKey = PackKey(cur.Robots, cur.Mask);
            if (!visited.TryGetValue(curKey, out int best) || cur.Cost != best) continue;

            if (cur.Mask == allKeysMask)
            {
                return cur.Cost;
            }

            for (int i = 0; i < NUM_ROBOTS; i++)
            {
                int robotNode = cur.Robots[i];
                var edges = keyGraph[robotNode];
                foreach (var e in edges)
                {
                    int target = e.To;
                    if ((cur.Mask & (1 << target)) != 0) continue;
                    if ((e.ReqMask & ~cur.Mask) != 0) continue;

                    int newMask = cur.Mask | (1 << target);
                    int newCost = cur.Cost + e.Dist;

                    int[] newRobots = (int[])cur.Robots.Clone();
                    newRobots[i] = target;

                    long newKey = PackKey(newRobots, newMask);
                    if (!visited.TryGetValue(newKey, out int oldCost) || newCost < oldCost)
                    {
                        visited[newKey] = newCost;
                        heap.Push(new State(newCost, newMask, newRobots));
                    }
                }
            }
        }

        return -1;
    }

    static long PackKey(int[] robots, int mask)
    {
        long key = mask;
        key = (key << 8) | (byte)(robots[0] & 0xFF);
        key = (key << 8) | (byte)(robots[1] & 0xFF);
        key = (key << 8) | (byte)(robots[2] & 0xFF);
        key = (key << 8) | (byte)(robots[3] & 0xFF);
        return key;
    }

    class State
    {
        public int Cost;
        public int Mask;
        public int[] Robots;
        public State(int cost, int mask, int[] robots)
        {
            Cost = cost;
            Mask = mask;
            Robots = robots;
        }
    }

    class MinHeap
    {
        private List<State> data = new List<State>();

        public bool IsEmpty => data.Count == 0;

        public void Push(State s)
        {
            data.Add(s);
            int i = data.Count - 1;
            while (i > 0)
            {
                int p = (i - 1) / 2;
                if (data[i].Cost < data[p].Cost)
                {
                    var tmp = data[i]; data[i] = data[p]; data[p] = tmp;
                    i = p;
                }
                else break;
            }
        }

        public State Pop()
        {
            var min = data[0];
            var last = data[data.Count - 1];
            data[0] = last;
            data.RemoveAt(data.Count - 1);
            int i = 0;
            while (true)
            {
                int l = i * 2 + 1;
                int r = l + 1;
                int smallest = i;
                if (l < data.Count && data[l].Cost < data[smallest].Cost) smallest = l;
                if (r < data.Count && data[r].Cost < data[smallest].Cost) smallest = r;
                if (smallest != i)
                {
                    var tmp = data[i]; data[i] = data[smallest]; data[smallest] = tmp;
                    i = smallest;
                }
                else break;
            }
            return min;
        }
    }
}