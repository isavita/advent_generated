
using System;
using System.Collections.Generic;
using System.IO;

class File
{
    public int size;
}

class Directory
{
    public Dictionary<string, File> files = new Dictionary<string, File>();
    public Dictionary<string, Directory> directories = new Dictionary<string, Directory>();
    
    public int totalSize()
    {
        int size = 0;
        foreach (var f in files)
        {
            size += f.Value.size;
        }
        foreach (var dir in directories)
        {
            size += dir.Value.totalSize();
        }
        return size;
    }
}

class Program
{
    static void Main()
    {
        Directory root = new Directory();
        Directory currentDir = root;
        List<Directory> directoryStack = new List<Directory> { root };

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                if (line.StartsWith("$ cd"))
                {
                    string path = line.Substring(4).Trim();
                    if (path == "/")
                    {
                        currentDir = root;
                        directoryStack = new List<Directory> { root };
                    }
                    else if (path == "..")
                    {
                        directoryStack.RemoveAt(directoryStack.Count - 1);
                        currentDir = directoryStack[directoryStack.Count - 1];
                    }
                    else
                    {
                        if (!currentDir.directories.ContainsKey(path))
                        {
                            currentDir.directories[path] = new Directory();
                        }
                        currentDir = currentDir.directories[path];
                        directoryStack.Add(currentDir);
                    }
                }
                else if (line.StartsWith("dir"))
                {
                    string dirName = line.Substring(4).Trim();
                    currentDir.directories[dirName] = new Directory();
                }
                else
                {
                    string[] parts = line.Split(' ', 2);
                    if (parts.Length == 2 && int.TryParse(parts[0], out int size))
                    {
                        string fileName = parts[1];
                        currentDir.files[fileName] = new File { size = size };
                    }
                }
            }
        }

        int sumSizes = 0;
        Action<Directory> calculateSizes = null;
        calculateSizes = (d) =>
        {
            int dirSize = d.totalSize();
            if (dirSize <= 100000)
            {
                sumSizes += dirSize;
            }
            foreach (var dir in d.directories)
            {
                calculateSizes(dir.Value);
            }
        };
        calculateSizes(root);

        Console.WriteLine(sumSizes);
    }
}
