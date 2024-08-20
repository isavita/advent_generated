import std.stdio;
import std.file;
import std.conv;
import std.string; // Import the string module

void main()
{
    auto file = File("input.txt", "r");
    auto cardPublicKey = to!long(file.readln().strip()); // Use strip to remove any whitespace or newline characters
    auto doorPublicKey = to!long(file.readln().strip()); // Use strip to remove any whitespace or newline characters
    file.close();

    auto cardLoopSize = findLoopSize(cardPublicKey);
    auto encryptionKey = transformSubjectNumber(doorPublicKey, cardLoopSize);

    writeln(encryptionKey);
}

long findLoopSize(long publicKey)
{
    long subjectNumber = 7;
    long value = 1;
    long loopSize = 0;

    while (value != publicKey)
    {
        value = (value * subjectNumber) % 20201227;
        loopSize++;
    }

    return loopSize;
}

long transformSubjectNumber(long subjectNumber, long loopSize)
{
    long value = 1;
    foreach (_; 0 .. loopSize)
    {
        value = (value * subjectNumber) % 20201227;
    }
    return value;
}