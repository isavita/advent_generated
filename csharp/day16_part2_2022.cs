
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Valve
{
    public string Id { get; set; }
    public int Flow { get; set; }
    public int Index { get; set; }
    public int OpenMaskIndex { get; set; } = -1;
}

public class Program
{
    private const int MaxValves = 60;
    private const int TimeLimit = 26;
    private const int Infinity = 1000000;

    private static Valve[] _valves = new Valve[MaxValves];
    private static int _numValves = 0;
    private static int[,] _dist = new int[MaxValves, MaxValves];
    private static int[] _openValveIndices = new int[MaxValves];
    private static int _numOpenValves = 0;
    private static int[,,] _memo = new int[MaxValves, TimeLimit + 1, 1 << 16];

    private static int FindValveIndex(string id)
    {
        for (int i = 0; i < _numValves; ++i)
        {
            if (_valves[i].Id == id)
                return i;
        }
        return -1;
    }

    private static int Max(int a, int b) => Math.Max(a, b);

    private static int Solve(int currIdx, int timeLeft, int openMask)
    {
        if (timeLeft <= 0) return 0;
        if (_memo[currIdx, timeLeft, openMask] != -1) return _memo[currIdx, timeLeft, openMask];

        int maxPressure = 0;

        for (int i = 0; i < _numOpenValves; ++i)
        {
            if (((openMask >> i) & 1) == 1)
            {
                int nextValveIdx = _openValveIndices[i];
                int travelTime = _dist[currIdx, nextValveIdx];
                int timeAfterAction = timeLeft - travelTime - 1;

                if (timeAfterAction > 0)
                {
                    int currentPressure = _valves[nextValveIdx].Flow * timeAfterAction;
                    int remainingMask = openMask & ~(1 << i);
                    maxPressure = Max(maxPressure, currentPressure + Solve(nextValveIdx, timeAfterAction, remainingMask));
                }
            }
        }

        _memo[currIdx, timeLeft, openMask] = maxPressure;
        return maxPressure;
    }

    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error: input.txt not found.");
            return;
        }

        string[] lines = File.ReadAllLines("input.txt");

        // Initialize distances
        for (int i = 0; i < MaxValves; ++i)
        {
            for (int j = 0; j < MaxValves; ++j)
                _dist[i, j] = i == j ? 0 : Infinity;
        }

        Dictionary<string, int> valveDict = new Dictionary<string, int>();

        foreach (string line in lines)
        {
            string[] parts = line.Split(new[] { " has flow rate=", "; tunnels lead to valves ", "; tunnel leads to valve " }, StringSplitOptions.None);

            string valveId = parts[0].Split(' ')[1];
            int flowRate = int.Parse(parts[1]);

            if (!valveDict.TryGetValue(valveId, out int currentIdx))
            {
                currentIdx = _numValves++;
                valveDict[valveId] = currentIdx;
                _valves[currentIdx] = new Valve { Id = valveId, Index = currentIdx };
            }

            _valves[currentIdx].Flow = flowRate;

            string[] neighbors = parts.Length == 3 ? parts[2].Split(", ") : new[] { parts[2] };

            foreach (string neighbor in neighbors)
            {
                if (!valveDict.TryGetValue(neighbor, out int neighborIdx))
                {
                    neighborIdx = _numValves++;
                    valveDict[neighbor] = neighborIdx;
                    _valves[neighborIdx] = new Valve { Id = neighbor, Index = neighborIdx };
                }

                _dist[currentIdx, neighborIdx] = 1;
            }
        }

        // Floyd-Warshall for all-pairs shortest paths
        for (int k = 0; k < _numValves; ++k)
        {
            for (int i = 0; i < _numValves; ++i)
            {
                for (int j = 0; j < _numValves; ++j)
                {
                    if (_dist[i, k] != Infinity && _dist[k, j] != Infinity)
                        _dist[i, j] = Math.Min(_dist[i, j], _dist[i, k] + _dist[k, j]);
                }
            }
        }

        // Identify valves with positive flow rate and assign mask indices
        _numOpenValves = 0;
        for (int i = 0; i < _numValves; ++i)
        {
            if (_valves[i].Flow > 0)
            {
                _valves[i].OpenMaskIndex = _numOpenValves;
                _openValveIndices[_numOpenValves++] = i;
            }
        }

        // Initialize memoization table
        for (int i = 0; i < MaxValves; ++i)
        {
            for (int j = 0; j <= TimeLimit; ++j)
            {
                for (int k = 0; k < (1 << 16); ++k)
                    _memo[i, j, k] = -1;
            }
        }

        int startIdx = Array.FindIndex(_valves, v => v != null && v.Id == "AA");
        if (startIdx == -1)
        {
            Console.WriteLine("Start valve 'AA' not found.");
            return;
        }

        int maxTotalPressure = 0;
        int totalMasks = (1 << _numOpenValves);

        for (int myMask = 0; myMask < totalMasks; ++myMask)
        {
            int elephantMask = (totalMasks - 1) ^ myMask;

            int myPressure = Solve(startIdx, TimeLimit, myMask);
            int elephantPressure = Solve(startIdx, TimeLimit, elephantMask);

            maxTotalPressure = Max(maxTotalPressure, myPressure + elephantPressure);
        }

        Console.WriteLine(maxTotalPressure);
    }
}
