using System;
using System.IO;
using System.Collections.Generic;

public class Program
{
    const long GEOLOGIC_Y = 16807L;
    const long GEOLOGIC_X = 48271L;
    const int CAVE_MODULO = 20183;

    const int TYPE_ROCKY = 0;
    const int TYPE_WET = 1;
    const int TYPE_NARROW = 2;

    const int TOOL_NONE = 1;
    const int TOOL_TORCH = 2;
    const int TOOL_GEAR = 4;
    const int MAX_TOOL_VALUE = 4;

    const int PADDING = 100;
    const int INF = int.MaxValue / 4;

    static int g_boundX, g_boundY;
    static int g_targetX, g_targetY;
    static int g_depth;

    static long[,] g_geoCache;
    static int[,] g_erosionCache;
    static int[,,] g_dist;

    static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        int depth = int.Parse(lines[0].Split(':')[1].Trim());
        var tPart = lines[1].Split(':')[1].Trim();
        var coords = tPart.Split(',');
        int targetX = int.Parse(coords[0]);
        int targetY = int.Parse(coords[1]);

        g_depth = depth;
        g_targetX = targetX;
        g_targetY = targetY;

        g_boundX = g_targetX + PADDING;
        g_boundY = g_targetY + PADDING;

        g_geoCache = new long[g_boundY, g_boundX];
        g_erosionCache = new int[g_boundY, g_boundX];
        g_dist = new int[g_boundY, g_boundX, MAX_TOOL_VALUE + 1];

        for (int y = 0; y < g_boundY; y++)
        {
            for (int x = 0; x < g_boundX; x++)
            {
                g_geoCache[y, x] = -1;
                g_erosionCache[y, x] = -1;
            }
        }

        for (int y = 0; y < g_boundY; y++)
        {
            for (int x = 0; x < g_boundX; x++)
            {
                GetErosionLevel(x, y);
            }
        }

        for (int y = 0; y < g_boundY; y++)
            for (int x = 0; x < g_boundX; x++)
                for (int t = 0; t <= MAX_TOOL_VALUE; t++)
                    g_dist[y, x, t] = INF;

        g_dist[0, 0, TOOL_TORCH] = 0;
        var heap = new MinHeap();

        heap.Push(new State(0, 0, 0, TOOL_TORCH));

        int[] dx = { 0, 0, 1, -1 };
        int[] dy = { 1, -1, 0, 0 };

        int finalTime = -1;

        while (heap.Count > 0)
        {
            var cur = heap.Pop();

            if (cur.Time > g_dist[cur.Y, cur.X, cur.Tool])
                continue;

            if (cur.X == g_targetX && cur.Y == g_targetY && cur.Tool == TOOL_TORCH)
            {
                finalTime = cur.Time;
                break;
            }

            int regionType = GetType(cur.X, cur.Y);
            int allowedHere = AllowedTools(regionType);

            foreach (int nextTool in new int[] { TOOL_NONE, TOOL_TORCH, TOOL_GEAR })
            {
                if ((nextTool & allowedHere) != 0 && nextTool != cur.Tool)
                {
                    int nt = cur.Time + 7;
                    if (nt < g_dist[cur.Y, cur.X, nextTool])
                    {
                        g_dist[cur.Y, cur.X, nextTool] = nt;
                        heap.Push(new State(nt, cur.X, cur.Y, nextTool));
                    }
                }
            }

            for (int dir = 0; dir < 4; dir++)
            {
                int nx = cur.X + dx[dir];
                int ny = cur.Y + dy[dir];

                if (nx < 0 || ny < 0 || nx >= g_boundX || ny >= g_boundY)
                    continue;

                int neighborType = GetType(nx, ny);
                int allowedThere = AllowedTools(neighborType);

                if ((cur.Tool & allowedThere) != 0)
                {
                    int nt = cur.Time + 1;
                    if (nt < g_dist[ny, nx, cur.Tool])
                    {
                        g_dist[ny, nx, cur.Tool] = nt;
                        heap.Push(new State(nt, nx, ny, cur.Tool));
                    }
                }
            }
        }

        Console.WriteLine(finalTime);
    }

    static int GetType(int x, int y)
    {
        return GetErosionLevel(x, y) % 3;
    }

    static int AllowedTools(int regionType)
    {
        switch (regionType)
        {
            case TYPE_ROCKY: return TOOL_GEAR | TOOL_TORCH;
            case TYPE_WET: return TOOL_GEAR | TOOL_NONE;
            case TYPE_NARROW: return TOOL_TORCH | TOOL_NONE;
            default: return 0;
        }
    }

    static int GetErosionLevel(int x, int y)
    {
        if (x < 0 || y < 0 || x >= g_boundX || y >= g_boundY) return -1;
        if (g_erosionCache[y, x] != -1) return g_erosionCache[y, x];

        long geoIndex = GetGeologicIndex(x, y);
        int level = (int)((geoIndex + g_depth) % CAVE_MODULO);
        g_erosionCache[y, x] = level;
        return level;
    }

    static long GetGeologicIndex(int x, int y)
    {
        if (x < 0 || y < 0 || x >= g_boundX || y >= g_boundY) return -1;
        if (g_geoCache[y, x] != -1) return g_geoCache[y, x];

        long index;
        if (x == 0 && y == 0)
            index = 0;
        else if (x == g_targetX && y == g_targetY)
            index = 0;
        else if (y == 0)
            index = (long)x * GEOLOGIC_Y;
        else if (x == 0)
            index = (long)y * GEOLOGIC_X;
        else
        {
            int erosionLeft = GetErosionLevel(x - 1, y);
            int erosionUp = GetErosionLevel(x, y - 1);
            index = (long)erosionLeft * erosionUp;
        }

        g_geoCache[y, x] = index;
        return index;
    }

    private class State
    {
        public int Time;
        public int X;
        public int Y;
        public int Tool;
        public State(int time, int x, int y, int tool)
        {
            Time = time;
            X = x;
            Y = y;
            Tool = tool;
        }
    }

    private class MinHeap
    {
        List<State> data = new List<State>();
        public int Count => data.Count;

        public void Push(State s)
        {
            data.Add(s);
            int i = data.Count - 1;
            while (i > 0)
            {
                int p = (i - 1) / 2;
                if (data[p].Time <= data[i].Time) break;
                var t = data[p];
                data[p] = data[i];
                data[i] = t;
                i = p;
            }
        }

        public State Pop()
        {
            State min = data[0];
            State last = data[data.Count - 1];
            data.RemoveAt(data.Count - 1);
            if (data.Count > 0)
            {
                data[0] = last;
                int i = 0;
                while (true)
                {
                    int l = i * 2 + 1;
                    int r = l + 1;
                    int smallest = i;
                    if (l < data.Count && data[l].Time < data[smallest].Time) smallest = l;
                    if (r < data.Count && data[r].Time < data[smallest].Time) smallest = r;
                    if (smallest != i)
                    {
                        var tmp = data[i];
                        data[i] = data[smallest];
                        data[smallest] = tmp;
                        i = smallest;
                    }
                    else break;
                }
            }
            return min;
        }
    }
}