import std.stdio;
import std.file;
import std.algorithm;

void main()
{
    auto file = File("input.txt", "r");

    int totalScore = 0;

    foreach(line; file.byLine())
    {
        char opponent = line[0];
        char yourMove = line[2];

        int score = 0;
        if(yourMove == 'X')
            score = 1;
        else if(yourMove == 'Y')
            score = 2;
        else if(yourMove == 'Z')
            score = 3;

        if((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X'))
            score += 6;
        else if(opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z')
            score += 3;

        totalScore += score;
    }

    writeln(totalScore);
}