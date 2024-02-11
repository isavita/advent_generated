
using System;
using System.IO;
using System.Linq;
using System.Text;

class Program
{
    static void Main()
    {
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                if (IsRealRoom(line))
                {
                    string decryptedName = DecryptName(line);
                    if (decryptedName.Contains("northpole object"))
                    {
                        Console.WriteLine(GetSectorID(line));
                        break;
                    }
                }
            }
        }
    }

    static bool IsRealRoom(string room)
    {
        string[] parts = room.Split('[');
        string checksum = parts[1].TrimEnd(']');
        string[] encryptedName = parts[0].Split('-').Take(parts[0].Split('-').Length - 1).ToArray();

        var letterCounts = encryptedName.SelectMany(part => part)
                                         .GroupBy(c => c)
                                         .Select(group => new { Letter = group.Key, Count = group.Count() })
                                         .OrderByDescending(x => x.Count)
                                         .ThenBy(x => x.Letter);

        for (int i = 0; i < checksum.Length; i++)
        {
            if (checksum[i] != letterCounts.ElementAt(i).Letter)
            {
                return false;
            }
        }

        return true;
    }

    static int GetSectorID(string room)
    {
        string[] parts = room.Split('-');
        string sectorIDPart = parts[parts.Length - 1];
        int sectorID = int.Parse(sectorIDPart.Split('[')[0]);
        return sectorID;
    }

    static string DecryptName(string room)
    {
        string[] parts = room.Split('-');
        string sectorIDPart = parts[parts.Length - 1];
        int sectorID = int.Parse(sectorIDPart.Split('[')[0]);
        StringBuilder decryptedName = new StringBuilder();

        foreach (string part in parts.Take(parts.Length - 1))
        {
            foreach (char letter in part)
            {
                if (letter == '-')
                {
                    decryptedName.Append(' ');
                }
                else
                {
                    char shiftedLetter = (char)('a' + ((letter - 'a' + sectorID) % 26));
                    decryptedName.Append(shiftedLetter);
                }
            }
            decryptedName.Append(' ');
        }

        return decryptedName.ToString().Trim();
    }
}
