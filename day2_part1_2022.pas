program RockPaperScissors;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

var
  inputFile: TextFile;
  line: string;
  opponent, response: char;
  totalScore, roundScore, responseScore, outcomeScore: Integer;

begin
  totalScore := 0;
  AssignFile(inputFile, 'input.txt');
  Reset(inputFile);
  
  while not Eof(inputFile) do
  begin
    ReadLn(inputFile, line);
    opponent := line[1];
    response := line[3];
    
    // Determine the score for the shape chosen
    case response of
      'X': responseScore := 1; // Rock
      'Y': responseScore := 2; // Paper
      'Z': responseScore := 3; // Scissors
    end;
    
    // Determine the outcome score
    if ((opponent = 'A') and (response = 'Y')) or ((opponent = 'B') and (response = 'Z')) or ((opponent = 'C') and (response = 'X')) then
      outcomeScore := 6 // Win
    else if ((opponent = 'A') and (response = 'X')) or ((opponent = 'B') and (response = 'Y')) or ((opponent = 'C') and (response = 'Z')) then
      outcomeScore := 3 // Draw
    else
      outcomeScore := 0; // Loss
    
    roundScore := responseScore + outcomeScore;
    totalScore := totalScore + roundScore;
  end;
  
  CloseFile(inputFile);
  
  WriteLn('Total score: ', totalScore);
end.
