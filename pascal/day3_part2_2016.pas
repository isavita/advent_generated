
program Solve;
uses SysUtils; // Often needed for file operations or Inc/Dec, though basic I/O and Inc are usually standard

var
  inputFile: Text;
  possible_triangles: LongInt; // Use LongInt for potentially large counts
  r1c1, r1c2, r1c3: Integer;
  r2c1, r2c2, r2c3: Integer;
  r3c1, r3c2, r3c3: Integer;
  s1, s2, s3, temp: Integer;
  j: Integer;

procedure CheckAndIncrement(a, b, c: Integer; var count: LongInt);
var
  t: Integer;
begin
  // Sort the three sides (a, b, c) ascendingly
  if a > b then begin t := a; a := b; b := t; end;
  if b > c then begin t := b; b := c; c := t; end;
  if a > b then begin t := a; a := b; b := t; end;

  // Check triangle inequality
  if a + b > c then
    Inc(count);
end;

begin
  Assign(inputFile, 'input.txt');
  Reset(inputFile);

  possible_triangles := 0;

  while not Eof(inputFile) do
  begin
    // Read the first row of the group
    if Eof(inputFile) then Break; // Check before reading
    Readln(inputFile, r1c1, r1c2, r1c3);

    // Read the second row of the group
    if Eof(inputFile) then Break; // Check before reading
    Readln(inputFile, r2c1, r2c2, r2c3);

    // Read the third row of the group
    if Eof(inputFile) then Break; // Check before reading
    Readln(inputFile, r3c1, r3c2, r3c3);

    // Now process the columns as triangles
    CheckAndIncrement(r1c1, r2c1, r3c1, possible_triangles); // First column
    CheckAndIncrement(r1c2, r2c2, r3c2, possible_triangles); // Second column
    CheckAndIncrement(r1c3, r2c3, r3c3, possible_triangles); // Third column
  end;

  Close(inputFile);

  Writeln(possible_triangles);
end.
