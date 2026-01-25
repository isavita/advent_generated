
using System;
using System.IO;
using System.Linq;

public class IntcodeComputer
{
    private long[] memory;
    private long ip;
    private long relativeBase;
    public bool Halted { get; private set; }
    public long? InputValue { get; set; }

    public IntcodeComputer(long[] program)
    {
        memory = new long[8192];
        Array.Copy(program, memory, program.Length);
        ip = 0;
        relativeBase = 0;
        Halted = false;
    }

    public long? Run()
    {
        while (true)
        {
            var instruction = memory[ip];
            var opcode = instruction % 100;
            var modes = new int[]
            {
                (int)((instruction / 100) % 10),
                (int)((instruction / 1000) % 10),
                (int)((instruction / 10000) % 10)
            };

            switch (opcode)
            {
                case 1:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        var destAddr = GetParamAddress(modes[2], 3);
                        memory[destAddr] = val1 + val2;
                        ip += 4;
                        break;
                    }
                case 2:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        var destAddr = GetParamAddress(modes[2], 3);
                        memory[destAddr] = val1 * val2;
                        ip += 4;
                        break;
                    }
                case 3:
                    {
                        if (!InputValue.HasValue)
                            return null; // Pause for input
                        var destAddr = GetParamAddress(modes[0], 1);
                        memory[destAddr] = InputValue.Value;
                        ip += 2;
                        InputValue = null;
                        break;
                    }
                case 4:
                    {
                        var output = GetParamValue(modes[0], 1);
                        ip += 2;
                        return output;
                    }
                case 5:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        if (val1 != 0)
                            ip = val2;
                        else
                            ip += 3;
                        break;
                    }
                case 6:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        if (val1 == 0)
                            ip = val2;
                        else
                            ip += 3;
                        break;
                    }
                case 7:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        var destAddr = GetParamAddress(modes[2], 3);
                        memory[destAddr] = val1 < val2 ? 1 : 0;
                        ip += 4;
                        break;
                    }
                case 8:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        var val2 = GetParamValue(modes[1], 2);
                        var destAddr = GetParamAddress(modes[2], 3);
                        memory[destAddr] = val1 == val2 ? 1 : 0;
                        ip += 4;
                        break;
                    }
                case 9:
                    {
                        var val1 = GetParamValue(modes[0], 1);
                        relativeBase += val1;
                        ip += 2;
                        break;
                    }
                case 99:
                    Halted = true;
                    return null;
                default:
                    throw new InvalidOperationException($"Unknown opcode {opcode} at ip {ip}");
            }
        }
    }

    private long GetParamAddress(int mode, int offset)
    {
        var param = memory[ip + offset];
        switch (mode)
        {
            case 0: return param;
            case 2: return relativeBase + param;
            default: throw new InvalidOperationException($"Unknown parameter mode {mode}");
        }
    }

    private long GetParamValue(int mode, int offset)
    {
        if (mode == 1)
            return memory[ip + offset];
        return memory[GetParamAddress(mode, offset)];
    }
}

public class Robot
{
    private IntcodeComputer computer;
    private (int x, int y) position;
    private int direction; // 0: Up, 1: Right, 2: Down, 3: Left
    private int[,] panels;
    private bool[,] painted;
    private int paintedCount;

    public Robot(long[] program, int startPanelColor)
    {
        computer = new IntcodeComputer(program);
        position = (100, 100);
        direction = 0; // Up
        panels = new int[200, 200];
        painted = new bool[200, 200];

        for (int y = 0; y < 200; y++)
        {
            for (int x = 0; x < 200; x++)
            {
                panels[y, x] = -1; // Unpainted
                painted[y, x] = false;
            }
        }

        panels[position.y, position.x] = startPanelColor;
    }

    public void Run()
    {
        bool expectingPaintColor = true;
        while (!computer.Halted)
        {
            var currentValue = panels[position.y, position.x];
            if (currentValue == -1) currentValue = 0; // Unpainted is black
            computer.InputValue = currentValue;

            var output = computer.Run();
            if (!output.HasValue) continue; // Waiting for input

            if (expectingPaintColor)
            {
                var paintColor = (int)output.Value;
                if (!painted[position.y, position.x])
                {
                    painted[position.y, position.x] = true;
                    paintedCount++;
                }
                panels[position.y, position.x] = paintColor;
                expectingPaintColor = false;
            }
            else
            {
                var turnDirection = (int)output.Value;
                TurnAndMove(turnDirection);
                expectingPaintColor = true;
            }
        }
    }

    private void TurnAndMove(int turnDirection)
    {
        direction = turnDirection == 0 ? (direction + 3) % 4 : (direction + 1) % 4;

        switch (direction)
        {
            case 0: position = (position.x, position.y - 1); break; // Up
            case 1: position = (position.x + 1, position.y); break; // Right
            case 2: position = (position.x, position.y + 1); break; // Down
            case 3: position = (position.x - 1, position.y); break; // Left
        }
    }

    public int PaintedCount => paintedCount;

    public void RenderPanels()
    {
        var minX = 200; var maxX = -1; var minY = 200; var maxY = -1;

        for (int y = 0; y < 200; y++)
        {
            for (int x = 0; x < 200; x++)
            {
                if (panels[y, x] != -1)
                {
                    if (x < minX) minX = x;
                    if (x > maxX) maxX = x;
                    if (y < minY) minY = y;
                    if (y > maxY) maxY = y;
                }
            }
        }

        if (maxX < minX)
        {
            Console.WriteLine("No panels painted.");
            return;
        }

        Console.WriteLine("\nRegistration Identifier:");
        for (int y = minY; y <= maxY; y++)
        {
            for (int x = minX; x <= maxX; x++)
            {
                var color = panels[y, x];
                if (color == -1) color = 0; // Treat unpainted as black for rendering
                Console.Write(color == 1 ? '#' : ' ');
            }
            Console.WriteLine();
        }
    }
}

class Program
{
    static void Main()
    {
        var input = File.ReadAllText("input.txt").Trim();
        var program = input.Split(',').Select(long.Parse).ToArray();

        var robot1 = new Robot(program, 0); // Start on black panel
        robot1.Run();
        Console.WriteLine($"Part One: {robot1.PaintedCount}");

        var robot2 = new Robot(program, 1); // Start on white panel
        robot2.Run();
        Console.WriteLine("Part Two:");
        robot2.RenderPanels();
    }
}
