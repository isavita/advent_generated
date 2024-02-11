
using System;
using System.Collections.Generic;
using System.IO;

class Cave
{
    private Dictionary<string, bool> connections;

    public Cave()
    {
        connections = new Dictionary<string, bool>();
    }

    public void ConnectTo(string name)
    {
        connections[name] = true;
    }

    public void DisconnectFrom(string name)
    {
        connections.Remove(name);
    }

    public Dictionary<string, bool> GetConnections()
    {
        return connections;
    }
}

class Program
{
    static void Main()
    {
        Dictionary<string, Cave> caves = new Dictionary<string, Cave>();
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] paths = line.Split('-');
                string from = paths[0];
                string to = paths[1];

                if (!caves.ContainsKey(from))
                {
                    caves[from] = new Cave();
                }

                if (!caves.ContainsKey(to))
                {
                    caves[to] = new Cave();
                }

                caves[from].ConnectTo(to);
                caves[to].ConnectTo(from);
            }
        }

        int count = 0;
        Action<string, Dictionary<string, bool>> dfs = null;
        dfs = (current, visited) =>
        {
            if (current == "end")
            {
                count++;
                return;
            }

            foreach (var next in caves[current].GetConnections())
            {
                if (visited.ContainsKey(next.Key) && next.Key.ToLower() == next.Key)
                {
                    continue;
                }

                Dictionary<string, bool> visitedCopy = new Dictionary<string, bool>(visited);
                visitedCopy[next.Key] = true;
                dfs(next.Key, visitedCopy);
            }
        };

        dfs("start", new Dictionary<string, bool> { { "start", true } });
        Console.WriteLine(count);
    }
}
