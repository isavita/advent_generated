
using System;
using System.IO;

class Program
{
    static void Main()
    {
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            int count = 0;
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] parts = line.Split(" | ");
                string output = parts[1];
                string[] digits = output.Split(" ");
                foreach (string digit in digits)
                {
                    switch (digit.Length)
                    {
                        case 2:
                        case 4:
                        case 3:
                        case 7:
                            count++;
                            break;
                    }
                }
            }

            Console.WriteLine(count);
        }
    }
}
