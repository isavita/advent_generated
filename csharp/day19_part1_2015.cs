
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        List<string> replacements = new List<string>();
        string molecule = "";

        foreach (string line in lines)
        {
            if (line == "")
                continue;
            if (line.Contains(" => "))
                replacements.Add(line);
            else
                molecule = line;
        }

        Dictionary<string, bool> molecules = new Dictionary<string, bool>();
        foreach (string replacement in replacements)
        {
            string[] parts = replacement.Split(new string[] { " => " }, StringSplitOptions.None);
            for (int i = 0; i < molecule.Length; i++)
            {
                if (molecule.Substring(i).StartsWith(parts[0]))
                {
                    string newMolecule = molecule.Substring(0, i) + parts[1] + molecule.Substring(i + parts[0].Length);
                    molecules[newMolecule] = true;
                }
            }
        }

        Console.WriteLine(molecules.Count);
    }
}
