
program Day15_ScienceForHungryPeople;

{$mode objfpc}{$H+} // Use Object Pascal mode and enable long strings

uses
  SysUtils; // Provides Int64, Val, file handling, string functions

const
  TotalTeaspoons = 100;
  MaxIngredients = 10; // Maximum number of ingredients supported

type
  // Record to store properties of a single ingredient
  TIngredient = record
    capacity, durability, flavor, texture, calories: Integer;
  end;

  // Array types for ingredients and their amounts
  TIngredientArray = array[1..MaxIngredients] of TIngredient;
  TAmountArray = array[1..MaxIngredients] of Integer;

var
  Ingredients: TIngredientArray; // Array to store ingredient data
  NumIngredients: Integer;       // Actual number of ingredients read
  MaxScore: Int64;               // Stores the maximum score found (use Int64 for large scores)
  CurrentAmounts: TAmountArray;  // Stores the amounts for the current combination being tested

// Helper function to return the maximum of two Int64 values
// Used to ensure property totals are not negative
function Max(a, b: Int64): Int64;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

// Function to extract the next integer value from a string, modifying the string
// Returns 0 if no integer is found or on conversion error
function ExtractNextInt(var s: string): Integer;
var
  p1, p2, code: Integer;
  valStr: string;
  numValue: Integer;
begin
  Result := 0; // Default return value
  p1 := 1;

  // Skip leading non-numeric characters (except potential minus sign)
  while (p1 <= Length(s)) and not (s[p1] in ['0'..'9', '-']) do
    Inc(p1);

  // Check if we found the start of a number
  if p1 <= Length(s) then
  begin
    p2 := p1;
    // Allow a single leading minus sign
    if s[p1] = '-' then
      Inc(p2);

    // Find the end of the number sequence
    while (p2 <= Length(s)) and (s[p2] in ['0'..'9']) do
      Inc(p2);

    // Extract the number string
    valStr := Copy(s, p1, p2 - p1);

    // Convert the string to an integer using Val for error checking
    Val(valStr, numValue, code);
    if code = 0 then // Successful conversion
      Result := numValue
    else
      Result := 0; // Conversion error, return 0

    // Update the input string to remove the processed part
    s := Copy(s, p2, Length(s));
  end
  else
  begin
    // No number found, clear the string to prevent infinite loops if called again
    s := '';
  end;
end;

// Procedure to parse a line from the input file and populate an Ingredient record
procedure ParseLine(const line: string; var ingredient: TIngredient);
var
  p: Integer;
  propertiesStr: string;
begin
  // Find the colon separating name from properties
  p := Pos(':', line);
  if p = 0 then Exit; // Invalid line format

  // Extract the part of the string containing the properties
  propertiesStr := Copy(line, p + 1, Length(line) - p);

  // Extract the 5 integer values using the helper function
  ingredient.capacity   := ExtractNextInt(propertiesStr);
  ingredient.durability := ExtractNextInt(propertiesStr);
  ingredient.flavor     := ExtractNextInt(propertiesStr);
  ingredient.texture    := ExtractNextInt(propertiesStr);
  ingredient.calories   := ExtractNextInt(propertiesStr);
end;

// Procedure to calculate the score for a given combination of amounts
procedure CalculateScore(const amounts: TAmountArray);
var
  i: Integer;
  totalCapacity, totalDurability, totalFlavor, totalTexture: Int64;
  currentScore: Int64;
begin
  totalCapacity := 0;
  totalDurability := 0;
  totalFlavor := 0;
  totalTexture := 0;

  // Calculate the total for each property based on ingredient amounts
  for i := 1 to NumIngredients do
  begin
    totalCapacity   := totalCapacity   + amounts[i] * Ingredients[i].capacity;
    totalDurability := totalDurability + amounts[i] * Ingredients[i].durability;
    totalFlavor     := totalFlavor     + amounts[i] * Ingredients[i].flavor;
    totalTexture    := totalTexture    + amounts[i] * Ingredients[i].texture;
    // Calories are ignored for score calculation
  end;

  // Ensure property totals are non-negative
  totalCapacity   := Max(0, totalCapacity);
  totalDurability := Max(0, totalDurability);
  totalFlavor     := Max(0, totalFlavor);
  totalTexture    := Max(0, totalTexture);

  // Calculate the final score by multiplying the non-negative property totals
  currentScore := totalCapacity * totalDurability * totalFlavor * totalTexture;

  // Update the global maximum score if the current score is higher
  if currentScore > MaxScore then
    MaxScore := currentScore;
end;

// Recursive procedure to generate all valid combinations of ingredient amounts
// k: the index of the ingredient currently being assigned an amount
// remainingTeaspoons: the number of teaspoons left to distribute
procedure FindBestCombination(k: Integer; remainingTeaspoons: Integer);
var
  i: Integer; // Amount to assign to ingredient k
begin
  // Base case: If this is the last ingredient
  if k = NumIngredients then
  begin
    // Assign all remaining teaspoons to the last ingredient
    // (The loop constraint ensures remainingTeaspoons >= 0)
    CurrentAmounts[k] := remainingTeaspoons;
    // Calculate the score for this complete combination
    CalculateScore(CurrentAmounts);
  end
  else // Recursive step: Assign amounts to ingredient k and recurse
  begin
    // Iterate through all possible amounts for the current ingredient (k)
    // from 0 up to the total remaining teaspoons
    for i := 0 to remainingTeaspoons do
    begin
      CurrentAmounts[k] := i; // Assign amount i to ingredient k
      // Recurse for the next ingredient (k+1) with updated remaining teaspoons
      FindBestCombination(k + 1, remainingTeaspoons - i);
    end;
  end;
end;

// Main program entry point
var
  InputFile: Text;
  line: string;

begin
  // --- Input Reading ---
  Assign(InputFile, 'input.txt');
  try
    Reset(InputFile);
  except
    on E: EInOutError do
    begin
      WriteLn('Error: Cannot open input file "input.txt". ', E.Message);
      Halt(1); // Exit program with an error code
    end;
  end;

  NumIngredients := 0;
  // Read each line from the input file
  while not Eof(InputFile) do
  begin
    ReadLn(InputFile, line);
    // Prevent exceeding the maximum ingredient limit
    if NumIngredients >= MaxIngredients then
    begin
      WriteLn('Error: Too many ingredients in input file (max=', MaxIngredients, ').');
      Close(InputFile);
      Halt(1);
    end;
    // Increment ingredient count and parse the line
    Inc(NumIngredients);
    ParseLine(line, Ingredients[NumIngredients]);
  end;
  Close(InputFile);

  // Check if any ingredients were read
  if NumIngredients = 0 then
  begin
    WriteLn('Error: No ingredients found in "input.txt".');
    Halt(1);
  end;

  // --- Score Calculation ---
  MaxScore := 0; // Initialize maximum score to zero

  // Start the recursive process to find the best combination
  // Begin with the first ingredient (index 1) and the total allowed teaspoons
  FindBestCombination(1, TotalTeaspoons);

  // --- Output ---
  // Print the maximum score found
  WriteLn(MaxScore);

// End of the main program block
end.
