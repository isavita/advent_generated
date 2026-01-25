
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class IntcodeState
{
    public long[] Mem { get; set; }
    public long Ip { get; set; }
    public long RelativeBase { get; set; }
    public bool Halted { get; set; }
    public long InputVal { get; set; }
    public long OutputVal { get; set; }

    public IntcodeState(long[] mem)
    {
        Mem = mem;
        Ip = 0;
        RelativeBase = 0;
        Halted = false;
    }

    public IntcodeState(IntcodeState other)
    {
        Mem = (long[])other.Mem.Clone();
        Ip = other.Ip;
        RelativeBase = other.RelativeBase;
        Halted = other.Halted;
    }
}

public class BFSState
{
    public int X { get; set; }
    public int Y { get; set; }
    public int Dist { get; set; }
    public IntcodeState Intcode { get; set; }
}

public class Program
{
    private const int MEM_SIZE = 8192;
    private const int MAP_DIM = 101;
    private const int MAP_OFFSET = MAP_DIM / 2;
    private const int MAX_QUEUE = MAP_DIM * MAP_DIM * 2;

    private enum TileType { Unknown = -1, Wall = 0, Floor = 1, Oxygen = 2 }
    private enum RunStatus { Ok, InputNeeded, OutputReady, Halted }

    private static TileType[,] map = new TileType[MAP_DIM, MAP_DIM];
    private static int[,] minDist = new int[MAP_DIM, MAP_DIM];
    private static Queue<BFSState> bfsQueue = new Queue<BFSState>();
    private static int oxygenX = -1, oxygenY = -1;
    private static int finalDistance = -1;

    public static void Main(string[] args)
    {
        Explore();
        Console.WriteLine(finalDistance);
    }

    private static void Explore()
    {
        InitMap();
        InitQueue();

        var initialIntcodeState = LoadIntcodeState("input.txt");
        var startX = MAP_OFFSET;
        var startY = MAP_OFFSET;

        map[startY, startX] = TileType.Floor;
        minDist[startY, startX] = 0;

        var initialBfsState = new BFSState
        {
            X = startX,
            Y = startY,
            Dist = 0,
            Intcode = initialIntcodeState
        };

        bfsQueue.Enqueue(initialBfsState);

        var dx = new[] { 0, 0, -1, 1 };
        var dy = new[] { -1, 1, 0, 0 };

        while (bfsQueue.Count > 0)
        {
            var current = bfsQueue.Dequeue();

            for (var moveCmd = 1; moveCmd <= 4; ++moveCmd)
            {
                var nextX = current.X + dx[moveCmd - 1];
                var nextY = current.Y + dy[moveCmd - 1];

                if (nextX < 0 || nextX >= MAP_DIM || nextY < 0 || nextY >= MAP_DIM) continue;

                var nextIntcodeState = new IntcodeState(current.Intcode);
                var runStatus = RunIntcode(nextIntcodeState, moveCmd);

                if (runStatus == RunStatus.OutputReady)
                {
                    var statusCode = (int)nextIntcodeState.OutputVal;
                    var nextDist = current.Dist + 1;

                    switch (statusCode)
                    {
                        case (int)TileType.Wall:
                            map[nextY, nextX] = TileType.Wall;
                            break;

                        case (int)TileType.Floor:
                        case (int)TileType.Oxygen:
                            if (nextDist < minDist[nextY, nextX])
                            {
                                map[nextY, nextX] = (TileType)statusCode;
                                minDist[nextY, nextX] = nextDist;

                                if (statusCode == (int)TileType.Oxygen)
                                {
                                    oxygenX = nextX;
                                    oxygenY = nextY;
                                    finalDistance = nextDist;
                                    return;
                                }

                                var nextBfsState = new BFSState
                                {
                                    X = nextX,
                                    Y = nextY,
                                    Dist = nextDist,
                                    Intcode = nextIntcodeState
                                };

                                bfsQueue.Enqueue(nextBfsState);
                            }
                            break;

                        default:
                            throw new Exception("Unknown status code");
                    }
                }
            }
        }
    }

    private static IntcodeState LoadIntcodeState(string filename)
    {
        var mem = File.ReadAllText(filename).Split(',').Select(long.Parse).ToArray();
        var state = new IntcodeState(mem);
        return state;
    }

    private static void InitMap()
    {
        for (var i = 0; i < MAP_DIM; i++)
        {
            for (var j = 0; j < MAP_DIM; j++)
            {
                map[i, j] = TileType.Unknown;
                minDist[i, j] = int.MaxValue;
            }
        }
    }

    private static void InitQueue()
    {
        bfsQueue.Clear();
    }

    private static RunStatus RunIntcode(IntcodeState state, long? input = null)
    {
        if (state.Halted) return RunStatus.Halted;

        while (state.Ip >= 0 && state.Ip < state.Mem.Length)
        {
            var instruction = state.Mem[state.Ip];
            var opcode = instruction % 100;
            var mode1 = (instruction / 100) % 10;
            var mode2 = (instruction / 1000) % 10;
            var mode3 = (instruction / 10000) % 10;

            switch (opcode)
            {
                case 1:
                    state.Mem[GetParamAddr(state, mode3, state.Mem[state.Ip + 3])] = GetParamVal(state, mode1, state.Mem[state.Ip + 1]) + GetParamVal(state, mode2, state.Mem[state.Ip + 2]);
                    state.Ip += 4;
                    break;

                case 2:
                    state.Mem[GetParamAddr(state, mode3, state.Mem[state.Ip + 3])] = GetParamVal(state, mode1, state.Mem[state.Ip + 1]) * GetParamVal(state, mode2, state.Mem[state.Ip + 2]);
                    state.Ip += 4;
                    break;

                case 3:
                    if (!input.HasValue) return RunStatus.InputNeeded;
                    state.Mem[GetParamAddr(state, mode1, state.Mem[state.Ip + 1])] = input.Value;
                    state.Ip += 2;
                    break;

                case 4:
                    state.OutputVal = GetParamVal(state, mode1, state.Mem[state.Ip + 1]);
                    state.Ip += 2;
                    return RunStatus.OutputReady;

                case 5:
                    if (GetParamVal(state, mode1, state.Mem[state.Ip + 1]) != 0)
                    {
                        state.Ip = GetParamVal(state, mode2, state.Mem[state.Ip + 2]);
                    }
                    else
                    {
                        state.Ip += 3;
                    }
                    break;

                case 6:
                    if (GetParamVal(state, mode1, state.Mem[state.Ip + 1]) == 0)
                    {
                        state.Ip = GetParamVal(state, mode2, state.Mem[state.Ip + 2]);
                    }
                    else
                    {
                        state.Ip += 3;
                    }
                    break;

                case 7:
                    state.Mem[GetParamAddr(state, mode3, state.Mem[state.Ip + 3])] = GetParamVal(state, mode1, state.Mem[state.Ip + 1]) < GetParamVal(state, mode2, state.Mem[state.Ip + 2]) ? 1 : 0;
                    state.Ip += 4;
                    break;

                case 8:
                    state.Mem[GetParamAddr(state, mode3, state.Mem[state.Ip + 3])] = GetParamVal(state, mode1, state.Mem[state.Ip + 1]) == GetParamVal(state, mode2, state.Mem[state.Ip + 2]) ? 1 : 0;
                    state.Ip += 4;
                    break;

                case 9:
                    state.RelativeBase += GetParamVal(state, mode1, state.Mem[state.Ip + 1]);
                    state.Ip += 2;
                    break;

                case 99:
                    state.Halted = true;
                    return RunStatus.Halted;

                default:
                    throw new Exception("Unknown opcode");
            }
        }

        throw new Exception("Instruction pointer out of bounds");
    }

    private static long GetParamVal(IntcodeState state, long mode, long param)
    {
        switch (mode)
        {
            case 0:
                return state.Mem[param];
            case 1:
                return param;
            case 2:
                return state.Mem[state.RelativeBase + param];
            default:
                throw new Exception("Invalid parameter mode");
        }
    }

    private static long GetParamAddr(IntcodeState state, long mode, long param)
    {
        switch (mode)
        {
            case 0:
                return param;
            case 2:
                return state.RelativeBase + param;
            default:
                throw new Exception("Invalid parameter mode for write address");
        }
    }
}
