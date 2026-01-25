using System;
using System.IO;

struct State
{
    public int x, y, dx, dy;
    public State(int x, int y, int dx, int dy) { this.x = x; this.y = y; this.dx = dx; this.dy = dy; }
}

class Program
{
    static int height, width;
    static char[,] grid;
    static int MaxQueueSize;

    static int SimulateBeam(int sx, int sy, int sdx, int sdy)
    {
        bool[,] energized = new bool[height, width];
        bool[,,] visited = new bool[height, width, 4];
        State[] queue = new State[MaxQueueSize];
        int head = 0, tail = 0;
        queue[tail++] = new State(sx, sy, sdx, sdy);

        while (head < tail)
        {
            State cur = queue[head++];
            int nx = cur.x + cur.dx;
            int ny = cur.y + cur.dy;
            if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;
            int dirIdx = cur.dx == 1 ? 0 : cur.dx == -1 ? 1 : cur.dy == 1 ? 2 : 3;
            if (visited[ny, nx, dirIdx]) continue;
            visited[ny, nx, dirIdx] = true;
            energized[ny, nx] = true;
            char cell = grid[ny, nx];
            if (cell == '.') { queue[tail++] = new State(nx, ny, cur.dx, cur.dy); }
            else if (cell == '/') { queue[tail++] = new State(nx, ny, -cur.dy, -cur.dx); }
            else if (cell == '\\') { queue[tail++] = new State(nx, ny, cur.dy, cur.dx); }
            else if (cell == '|')
            {
                if (cur.dx != 0)
                {
                    queue[tail++] = new State(nx, ny, 0, 1);
                    queue[tail++] = new State(nx, ny, 0, -1);
                }
                else queue[tail++] = new State(nx, ny, cur.dx, cur.dy);
            }
            else if (cell == '-')
            {
                if (cur.dy != 0)
                {
                    queue[tail++] = new State(nx, ny, 1, 0);
                    queue[tail++] = new State(nx, ny, -1, 0);
                }
                else queue[tail++] = new State(nx, ny, cur.dx, cur.dy);
            }
        }
        int count = 0;
        for (int y = 0; y < height; y++) for (int x = 0; x < width; x++) if (energized[y, x]) count++;
        return count;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        height = lines.Length;
        width = lines[0].Length;
        grid = new char[height, width];
        for (int y = 0; y < height; y++) for (int x = 0; x < width; x++) grid[y, x] = lines[y][x];
        MaxQueueSize = height * width * 4;
        int max = 0, c;
        for (int x = 0; x < width; x++) { c = SimulateBeam(x, -1, 0, 1); if (c > max) max = c; }
        for (int x = 0; x < width; x++) { c = SimulateBeam(x, height, 0, -1); if (c > max) max = c; }
        for (int y = 0; y < height; y++) { c = SimulateBeam(-1, y, 1, 0); if (c > max) max = c; }
        for (int y = 0; y < height; y++) { c = SimulateBeam(width, y, -1, 0); if (c > max) max = c; }
        Console.WriteLine(max);
    }
}