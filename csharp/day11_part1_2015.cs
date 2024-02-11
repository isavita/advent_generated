
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string currentPassword = File.ReadAllText("input.txt").Trim();
        string newPassword = FindNextPassword(currentPassword);
        Console.WriteLine(newPassword);
    }

    static string FindNextPassword(string password)
    {
        do
        {
            password = IncrementPassword(password);
        } while (!IsValidPassword(password));
        return password;
    }

    static string IncrementPassword(string password)
    {
        char[] chars = password.ToCharArray();
        for (int i = chars.Length - 1; i >= 0; i--)
        {
            chars[i]++;
            if (chars[i] > 'z')
            {
                chars[i] = 'a';
            }
            else
            {
                break;
            }
        }
        return new string(chars);
    }

    static bool IsValidPassword(string password)
    {
        return HasStraight(password) && !ContainsInvalidLetters(password) && HasTwoPairs(password);
    }

    static bool HasStraight(string password)
    {
        for (int i = 0; i < password.Length - 2; i++)
        {
            if (password[i] + 1 == password[i + 1] && password[i] + 2 == password[i + 2])
            {
                return true;
            }
        }
        return false;
    }

    static bool ContainsInvalidLetters(string password)
    {
        return password.Contains('i') || password.Contains('o') || password.Contains('l');
    }

    static bool HasTwoPairs(string password)
    {
        int count = 0;
        for (int i = 0; i < password.Length - 1; i++)
        {
            if (password[i] == password[i + 1])
            {
                count++;
                i++; // Skip the next character
            }
        }
        return count >= 2;
    }
}
