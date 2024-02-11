
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int[] inputData = lines.SelectMany(line => line.Split(',')).Select(int.Parse).ToArray();

        inputData[1] = 12;
        inputData[2] = 2;

        int result = ExecuteProgram(inputData);

        Console.WriteLine(result);
    }

    static int ExecuteProgram(int[] data)
    {
        for (int i = 0; i < data.Length - 3; i += 4)
        {
            int pos1 = data[i + 1];
            int pos2 = data[i + 2];
            int pos3 = data[i + 3];

            switch (data[i])
            {
                case 1:
                    data[pos3] = data[pos1] + data[pos2];
                    break;
                case 2:
                    data[pos3] = data[pos1] * data[pos2];
                    break;
                case 99:
                    return data[0];
                default:
                    throw new InvalidOperationException("Invalid opcode");
            }
        }

        return data[0];
    }
}
