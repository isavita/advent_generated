
program CookieRecipe;

{$MODE OBJFPC}{$H+} // Use Free Pascal extensions

uses
  SysUtils, Classes;

const
  TotalTeaspoons = 100;
  TargetCalories = 500;
  InputFileName = 'input.txt';

type
  Ingredient = record
    Name: string;
    Capacity: Integer;
    Durability: Integer;
    Flavor: Integer;
    Texture: Integer;
    Calories: Integer;
  end;

  IngredientList = array of Ingredient;
  TeaspoonList = array of Integer;

var
  Ingredients: IngredientList;
  MaxScoreValue: Int64;

function ExtractInt(const S: string): Integer;
var
  ErrorCode: Integer;
  Value: Integer;
begin
  Val(StringReplace(S, ',', '', [rfReplaceAll]), Value, ErrorCode);
  if ErrorCode <> 0 then
    Value := 0; // Or raise an exception
  Result := Value;
end;

procedure ReadIngredients(const Filename: string; var ingredients: IngredientList);
var
  F: TextFile;
  line: string;
  parts: TStringList;
  count: Integer;
  NewIngredient: Ingredient;
begin
  if not FileExists(Filename) then
  begin
     WriteLn('Error: Input file not found: ', Filename);
     Halt(1);
  end;

  AssignFile(F, Filename);
  Reset(F);
  count := 0;
  SetLength(ingredients, 0);
  parts := TStringList.Create;
  parts.Delimiter := ' ';
  parts.StrictDelimiter := True;

  try
    while not Eof(F) do
    begin
      ReadLn(F, line);
      parts.DelimitedText := line;

      if parts.Count >= 11 then
      begin
        Inc(count);
        SetLength(ingredients, count);
        NewIngredient.Name := parts[0]; // Name might have ':' at the end, ignored here
        NewIngredient.Capacity := ExtractInt(parts[2]);
        NewIngredient.Durability := ExtractInt(parts[4]);
        NewIngredient.Flavor := ExtractInt(parts[6]);
        NewIngredient.Texture := ExtractInt(parts[8]);
        NewIngredient.Calories := ExtractInt(parts[10]);
        ingredients[count - 1] := NewIngredient;
      end;
    end;
  finally
    parts.Free;
    CloseFile(F);
  end;
end;

function CalculateScore(const ingredients: IngredientList; const teaspoons: TeaspoonList): Int64;
var
  i: Integer;
  capacity, durability, flavor, texture: Int64;
begin
  capacity := 0;
  durability := 0;
  flavor := 0;
  texture := 0;
  for i := 0 to High(ingredients) do
  begin
    capacity := capacity + Int64(ingredients[i].Capacity) * teaspoons[i];
    durability := durability + Int64(ingredients[i].Durability) * teaspoons[i];
    flavor := flavor + Int64(ingredients[i].Flavor) * teaspoons[i];
    texture := texture + Int64(ingredients[i].Texture) * teaspoons[i];
  end;

  if capacity < 0 then capacity := 0;
  if durability < 0 then durability := 0;
  if flavor < 0 then flavor := 0;
  if texture < 0 then texture := 0;

  Result := capacity * durability * flavor * texture;
end;

function CalculateCalories(const ingredients: IngredientList; const teaspoons: TeaspoonList): Integer;
var
  i, calories: Integer;
begin
  calories := 0;
  for i := 0 to High(ingredients) do
  begin
    calories := calories + ingredients[i].Calories * teaspoons[i];
  end;
  Result := calories;
end;

function FindMaxScoreRecursive(const ingredients: IngredientList; index: Integer; remaining: Integer; var teaspoons: TeaspoonList; targetCalories: Integer): Int64;
var
  i: Integer;
  maxScore, currentScore: Int64;
begin
  if index = High(ingredients) then
  begin
    teaspoons[index] := remaining;
    if CalculateCalories(ingredients, teaspoons) = targetCalories then
    begin
      Result := CalculateScore(ingredients, teaspoons);
    end
    else
    begin
      Result := 0;
    end;
  end
  else
  begin
    maxScore := 0;
    for i := 0 to remaining do
    begin
      teaspoons[index] := i;
      currentScore := FindMaxScoreRecursive(ingredients, index + 1, remaining - i, teaspoons, targetCalories);
      if currentScore > maxScore then
      begin
        maxScore := currentScore;
      end;
    end;
    Result := maxScore;
  end;
end;

function FindMaxScore(const ingredients: IngredientList; totalTeaspoons: Integer; targetCalories: Integer): Int64;
var
  teaspoons: TeaspoonList;
begin
   if Length(ingredients) = 0 then
     Exit(0);

   SetLength(teaspoons, Length(ingredients));
   Result := FindMaxScoreRecursive(ingredients, 0, totalTeaspoons, teaspoons, targetCalories);
end;

begin
  ReadIngredients(InputFileName, Ingredients);
  if Length(Ingredients) = 0 then
  begin
      WriteLn('No valid ingredients found in ', InputFileName);
      Halt(1);
  end;
  MaxScoreValue := FindMaxScore(Ingredients, TotalTeaspoons, TargetCalories);
  WriteLn(MaxScoreValue);
end.
