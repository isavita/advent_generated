
program FindAuntSue;

uses
  SysUtils; // For Trim, possibly Val depending on Pascal version

type
  TAuntSue = record
    children: Integer;
    cats: Integer;
    samoyeds: Integer;
    pomeranians: Integer;
    akitas: Integer;
    vizslas: Integer;
    goldfish: Integer;
    trees: Integer;
    cars: Integer;
    perfumes: Integer;
  end;

const
  TargetSue: TAuntSue = (
    children: 3; cats: 7; samoyeds: 2; pomeranians: 3; akitas: 0;
    vizslas: 0; goldfish: 5; trees: 3; cars: 2; perfumes: 1
  );
  InputFileName = 'input.txt';

var
  f: TextFile;
  line, propStr, currentPair, propName, valueStr: string;
  sueNum, propValue, code, p1, p2: Integer;
  isValid: Boolean;

begin
  Assign(f, InputFileName);
  Reset(f);

  while not Eof(f) do
  begin
    ReadLn(f, line);

    p1 := Pos(' ', line);          // Find first space (after "Sue")
    p2 := Pos(':', line);          // Find first colon (after number)
    Val(Copy(line, p1 + 1, p2 - p1 - 1), sueNum, code); // Extract Sue number

    propStr := Trim(Copy(line, p2 + 1, Length(line) - p2)); // Extract properties part

    isValid := True;
    while (Length(propStr) > 0) and isValid do
    begin
      p1 := Pos(',', propStr);
      if p1 = 0 then // Last or only property
      begin
        currentPair := Trim(propStr);
        propStr := '';
      end
      else
      begin
        currentPair := Trim(Copy(propStr, 1, p1 - 1));
        propStr := Trim(Copy(propStr, p1 + 1, Length(propStr)));
      end;

      p2 := Pos(':', currentPair);
      propName := Trim(Copy(currentPair, 1, p2 - 1));
      valueStr := Trim(Copy(currentPair, p2 + 1, Length(currentPair)));
      Val(valueStr, propValue, code);

      case propName of
        'children':
          if propValue <> TargetSue.children then isValid := False;
        'cats':
          if propValue <= TargetSue.cats then isValid := False;
        'samoyeds':
          if propValue <> TargetSue.samoyeds then isValid := False;
        'pomeranians':
          if propValue >= TargetSue.pomeranians then isValid := False;
        'akitas':
          if propValue <> TargetSue.akitas then isValid := False;
        'vizslas':
          if propValue <> TargetSue.vizslas then isValid := False;
        'goldfish':
          if propValue >= TargetSue.goldfish then isValid := False;
        'trees':
          if propValue <= TargetSue.trees then isValid := False;
        'cars':
          if propValue <> TargetSue.cars then isValid := False;
        'perfumes':
          if propValue <> TargetSue.perfumes then isValid := False;
      end; // case
    end; // while properties

    if isValid then
    begin
      WriteLn(sueNum);
      // In this specific problem, only one Sue matches, so we could exit.
      // However, the original Python checks all, so we do too.
      // If optimization to exit early was desired: Close(f); Exit;
    end;
  end; // while not Eof

  Close(f);
end.
