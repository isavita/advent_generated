
program InfiniteElves;

{$MODE OBJFPC}{$H+} // Use Free Pascal mode with LongInt support

uses
  SysUtils; // For Potential StrToInt conversion if needed, though not strictly required here.

const
  // Estimate a reasonable upper bound for the house number.
  // The target number divided by 10 gives a lower bound on the sum of divisors.
  // House numbers are often roughly similar to their sum of divisors,
  // but can be smaller for highly composite numbers.
  // Let's try 1 million houses. Increase if the target is not found.
  // For a target like 30,000,000, sum_divisors * 10 >= 30M => sum_divisors >= 3M
  // The house number might be around 1M.
  MaxHouses = 1000000;

var
  InputFile: Text;
  TargetPresents: LongInt;
  Presents: array[1..MaxHouses] of LongInt; // Use LongInt as present counts can exceed 32767
  Elf, House, PresentsToAdd: LongInt; // Use LongInt for indices if MaxHouses is large
  ResultHouse: LongInt;
  i: Integer; // Loop variable for initialization, can be Integer if MaxHouses fits

begin
  // --- Input Reading ---
  Assign(InputFile, 'input.txt');
  try
    Reset(InputFile);
    ReadLn(InputFile, TargetPresents);
  except
    on E: EInOutError do
    begin
      WriteLn('Error: Cannot read from input.txt. Make sure the file exists and contains a number.');
      Halt(1); // Terminate program with an error code
    end;
  end;
  Close(InputFile);

  // --- Initialization ---
  // Initialize the presents array to zero.
  // FillChar might be faster but requires careful handling of types.
  // A loop is safer and clearer for standard Pascal.
  for i := 1 to MaxHouses do
  begin
    Presents[i] := 0;
  end;

  // --- Elf Simulation ---
  // Iterate through each elf (up to the MaxHouses limit, as elves only
  // contribute to houses with numbers greater than or equal to their own)
  for Elf := 1 to MaxHouses do
  begin
    PresentsToAdd := Elf * 10; // Calculate presents delivered by this elf

    // Elf visits houses which are multiples of its number
    House := Elf;
    while House <= MaxHouses do
    begin
      // Add presents to the current house
      // Check for potential overflow, although LongInt should be sufficient for typical AoC inputs
      if High(LongInt) - Presents[House] < PresentsToAdd then
      begin
         WriteLn('Error: Potential LongInt overflow at house ', House);
         Halt(1);
      end;
      Presents[House] := Presents[House] + PresentsToAdd;

      // Move to the next house the elf visits
      House := House + Elf;
    end;
  end;

  // --- Find the First House ---
  // Iterate through the houses to find the first one meeting the target
  ResultHouse := -1; // Sentinel value indicating not found yet
  House := 1;
  while (House <= MaxHouses) and (ResultHouse = -1) do
  begin
    if Presents[House] >= TargetPresents then
    begin
      ResultHouse := House; // Found the house
    end
    else
    begin
      Inc(House); // Check next house
    end;
  end;

  // --- Output Result ---
  if ResultHouse <> -1 then
  begin
    WriteLn(ResultHouse);
  end
  else
  begin
    // This indicates our MaxHouses constant might be too small
    WriteLn('Error: No house found within the limit of ', MaxHouses, ' houses that meets the target of ', TargetPresents, ' presents.');
    WriteLn('Consider increasing the MaxHouses constant in the source code.');
    Halt(1); // Indicate failure
  end;

// Program successfully completes if a result was found and printed.
// The final 'end.' marks the end of the main program block.
end.
