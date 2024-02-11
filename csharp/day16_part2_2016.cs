
using System;
using System.IO;
using System.Text;

class Program
{
    const int DiskLength = 35651584;

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
                if (data[i] == '0')
                {
                    sb.Append('1');
                }
                else
                {
                    sb.Append('0');
                }
            }
            data = data + "0" + sb.ToString();
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
                if (data[i] == data[i + 1])
                {
                    sb.Append('1');
                }
                else
                {
                    sb.Append('0');
                }
            }
            data = sb.ToString();
        }
        return data;
    }
}
