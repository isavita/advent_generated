
using System;
using System.IO;
using System.Text;

class Program
{
    const int DiskLength = 272;

    static void Main()
    {
        string initialState = ReadInitialState("input.txt");
        string data = GenerateData(initialState, DiskLength);
        string checksum = CalculateChecksum(data);
        Console.WriteLine("Checksum: " + checksum);
    }

    static string ReadInitialState(string filename)
    {
        using (StreamReader reader = new StreamReader(filename))
        {
            return reader.ReadLine();
        }
    }

    static string GenerateData(string initialState, int length)
    {
        string data = initialState;
        while (data.Length < length)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = data.Length - 1; i >= 0; i--)
            {
                sb.Append(data[i] == '0' ? '1' : '0');
            }
            data += "0" + sb.ToString();
        }
        return data.Substring(0, length);
    }

    static string CalculateChecksum(string data)
    {
        while (data.Length % 2 == 0)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < data.Length; i += 2)
            {
                sb.Append(data[i] == data[i + 1] ? '1' : '0');
            }
            data = sb.ToString();
        }
        return data;
    }
}
