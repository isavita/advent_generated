import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.conv;
import std.string;
import std.typecons;

void main()
{
    auto lines = readText("input.txt").splitter("\n").array;
    lines.sort!((a, b) => a < b);

    int[string] guardSleepMinutes;
    int[string][string] guardMinuteFrequency;

    string currentGuard;
    int sleepStart;

    foreach (line; lines)
    {
        if (line.canFind("Guard"))
        {
            auto guardIdPos = line.indexOf("#") + 1;
            auto guardIdEnd = line.indexOf(" ", guardIdPos);
            currentGuard = line[guardIdPos .. guardIdEnd];
        }
        else if (line.canFind("falls asleep"))
        {
            sleepStart = to!int(line[15 .. 17]);
        }
        else if (line.canFind("wakes up"))
        {
            int wakeUp = to!int(line[15 .. 17]);
            guardSleepMinutes[currentGuard] += wakeUp - sleepStart;
            foreach (i; sleepStart .. wakeUp)
            {
                guardMinuteFrequency[currentGuard][to!string(i)]++;
            }
        }
    }

    string chosenGuard;
    int maxMinuteFreq = 0;
    int chosenMinute = 0;

    foreach (guard, minutes; guardMinuteFrequency)
    {
        foreach (minute, freq; minutes)
        {
            if (freq > maxMinuteFreq)
            {
                maxMinuteFreq = freq;
                chosenGuard = guard;
                chosenMinute = to!int(minute);
            }
        }
    }

    writeln(to!int(chosenGuard) * chosenMinute);
}