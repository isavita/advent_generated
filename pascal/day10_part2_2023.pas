
program PipeMaze;

{$mode objfpc}{$H+} // Use Object Pascal mode and enable extended syntax

uses
  SysUtils, Classes, Math;

type
  // Represents (x, y) coordinates
  TCoord = record
    X, Y: Integer;
  end;

  // Represents directions
  TDirection = (dirTop, dirRight, dirBottom, dirLeft, dirUndefined);

const
  // Coordinate offsets for each direction
  DirOffsets: array[TDirection] of TCoord = (
    (X: 0; Y: -1), // dirTop
    (X: 1; Y: 0),  // dirRight
    (X: 0; Y: 1),  // dirBottom
    (X: -1; Y: 0), // dirLeft
    (X: 0; Y: 0)   // dirUndefined
  );

  // Pipe characters
  PipeV = '|';
  PipeH = '-';
  PipeTL = 'J';
  PipeTR = 'L';
  PipeBL = '7';
  PipeBR = 'F';
  Ground = '.';
  Start = 'S';

type
  // Set of directions a pipe connects to
  TPipeConnections = set of TDirection;

  // Grid representation
  TGrid = array of array of Char;
  // Path representation (dynamic array of coordinates)
  TPath = array of TCoord;
  // Boolean grid to mark path tiles
  TPathGrid = array of array of Boolean;

// --- Coordinate Helper Functions ---

function CreateCoord(AX, AY: Integer): TCoord;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function AddCoords(A, B: TCoord): TCoord;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function CoordsEqual(A, B: TCoord): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

// --- Direction Helper Functions ---

function OppositeDir(Dir: TDirection): TDirection;
begin
  case Dir of
    dirTop: Result := dirBottom;
    dirRight: Result := dirLeft;
    dirBottom: Result := dirTop;
    dirLeft: Result := dirRight;
  else
    Result := dirUndefined;
  end;
end;

// --- Pipe Logic Functions ---

// Get the set of directions a pipe tile connects to
function GetPipeConnections(Tile: Char): TPipeConnections;
begin
  Result := []; // Empty set
  case Tile of
    PipeV: Result := [dirTop, dirBottom];
    PipeH: Result := [dirLeft, dirRight];
    PipeTL: Result := [dirTop, dirLeft];
    PipeTR: Result := [dirTop, dirRight];
    PipeBL: Result := [dirBottom, dirLeft];
    PipeBR: Result := [dirBottom, dirRight];
  end;
end;

// Get the tile character corresponding to a set of connections
function GetTileFromPipe(Connections: TPipeConnections): Char;
begin
  Result := Ground; // Default to ground
  if Connections = [dirTop, dirBottom] then Result := PipeV
  else if Connections = [dirLeft, dirRight] then Result := PipeH
  else if Connections = [dirTop, dirLeft] then Result := PipeTL
  else if Connections = [dirTop, dirRight] then Result := PipeTR
  else if Connections = [dirBottom, dirLeft] then Result := PipeBL
  else if Connections = [dirBottom, dirRight] then Result := PipeBR;
end;

// --- Grid and Path Functions ---

var
  Grid: TGrid;
  PathGrid: TPathGrid;
  StartCoord: TCoord;
  MaxY, MaxX: Integer;

// Read input file and build the grid
procedure ReadInputAndBuildGrid(const Filename: string);
var
  InputFile: TextFile;
  Line: string;
  Y: Integer;
  X: Integer;
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    AssignFile(InputFile, Filename);
    Reset(InputFile);
    Y := 0;
    StartCoord := CreateCoord(-1, -1); // Initialize StartCoord
    while not Eof(InputFile) do
    begin
      ReadLn(InputFile, Line);
      Lines.Add(Line);
      if StartCoord.X = -1 then // Search for Start 'S'
      begin
        for X := 0 to Length(Line) - 1 do
        begin
          if Line[X + 1] = Start then
          begin
            StartCoord := CreateCoord(X, Y);
          end;
        end;
      end;
      Inc(Y);
    end;
    CloseFile(InputFile);

    MaxY := Lines.Count;
    if MaxY > 0 then MaxX := Length(Lines[0]) else MaxX := 0;

    SetLength(Grid, MaxY, MaxX);
    SetLength(PathGrid, MaxY, MaxX); // Initialize PathGrid

    for Y := 0 to MaxY - 1 do
    begin
      Line := Lines[Y];
      for X := 0 to MaxX - 1 do
      begin
        if (X < Length(Line)) then
          Grid[Y, X] := Line[X + 1]
        else
          Grid[Y, X] := Ground; // Handle potentially uneven lines
        PathGrid[Y, X] := False; // Initialize PathGrid cells
      end;
    end;

  finally
    Lines.Free;
  end;
end;

// Determine the connections for the Start 'S' tile based on neighbors
function DetermineStartPipe(StartPos: TCoord): TPipeConnections;
var
  Dir: TDirection;
  NeighborCoord: TCoord;
  NeighborConnections: TPipeConnections;
begin
  Result := [];
  for Dir := Low(TDirection) to High(TDirection) do
  begin
    if Dir = dirUndefined then Continue;

    NeighborCoord := AddCoords(StartPos, DirOffsets[Dir]);

    // Check bounds
    if (NeighborCoord.Y >= 0) and (NeighborCoord.Y < MaxY) and
       (NeighborCoord.X >= 0) and (NeighborCoord.X < MaxX) then
    begin
      NeighborConnections := GetPipeConnections(Grid[NeighborCoord.Y, NeighborCoord.X]);
      // Check if neighbor connects back to Start
      if OppositeDir(Dir) in NeighborConnections then
      begin
        Include(Result, Dir);
      end;
    end;
  end;
end;

// Find the main loop path starting from StartCoord
function FindPath(StartPos: TCoord; StartConnections: TPipeConnections): TPath;
var
  Current, NextCoord: TCoord;
  PrevDir, CurrentDir: TDirection;
  CurrentConnections: TPipeConnections;
  PathIndex: Integer;
  FirstDirFound: Boolean;
begin
  SetLength(Result, 0); // Initialize empty path
  SetLength(Result, Length(Result) + 1);
  Result[0] := StartPos;

  Current := StartPos;
  PrevDir := dirUndefined;
  FirstDirFound := False;

  // Find the first step direction from Start
  for CurrentDir := Low(TDirection) to High(TDirection) do
  begin
    if CurrentDir in StartConnections then
    begin
        PrevDir := CurrentDir; // This is the direction *from* start *to* the next pipe
        Current := AddCoords(StartPos, DirOffsets[CurrentDir]);
        FirstDirFound := True;
        break;
    end;
  end;

  if not FirstDirFound then Exit; // Should not happen with valid input

  PathIndex := 1;
  while not CoordsEqual(Current, StartPos) do
  begin
    SetLength(Result, Length(Result) + 1);
    Result[PathIndex] := Current;
    Inc(PathIndex);

    CurrentConnections := GetPipeConnections(Grid[Current.Y, Current.X]);

    // Find the next direction (not going back)
    for CurrentDir := Low(TDirection) to High(TDirection) do
    begin
       if (CurrentDir <> dirUndefined) and (CurrentDir in CurrentConnections) and (CurrentDir <> OppositeDir(PrevDir)) then
       begin
         PrevDir := CurrentDir; // Direction we are moving in now
         Current := AddCoords(Current, DirOffsets[CurrentDir]);
         break;
       end;
    end;
  end;
end;

// Mark the path on the PathGrid and replace 'S' in the main Grid
procedure MarkPathAndReplaceStart(Path: TPath; StartConnections: TPipeConnections);
var
  I: Integer;
  Coord: TCoord;
begin
  for I := 0 to Length(Path) - 1 do
  begin
    Coord := Path[I];
    PathGrid[Coord.Y, Coord.X] := True;
  end;
  // Replace 'S' with its actual pipe type
  Grid[StartCoord.Y, StartCoord.X] := GetTileFromPipe(StartConnections);
end;

// Check if a coordinate is inside the loop using ray casting (horizontal)
function IsInside(Coord: TCoord): Boolean;
var
  X: Integer;
  Crossings: Integer;
  Tile: Char;
  OnUpperEdge: Boolean; // True if currently tracking along an upper edge (L)
  OnLowerEdge: Boolean; // True if currently tracking along a lower edge (F)
begin
  Result := False;
  // If the point is on the path itself, it's not strictly inside
  if PathGrid[Coord.Y, Coord.X] then Exit;

  Crossings := 0;
  OnUpperEdge := False;
  OnLowerEdge := False;

  for X := 0 to Coord.X - 1 do
  begin
    if PathGrid[Coord.Y, X] then // Only consider path tiles
    begin
      Tile := Grid[Coord.Y, X];
      case Tile of
        PipeV: Inc(Crossings); // Vertical pipe always counts as a crossing
        PipeH: ; // Horizontal pipes don't change crossing count directly
        PipeTR: OnUpperEdge := True; // Start of an upper edge segment
        PipeBR: OnLowerEdge := True; // Start of a lower edge segment
        PipeTL: // End of an edge segment
          begin
            if OnLowerEdge then Inc(Crossings); // F--J counts as crossing
            OnUpperEdge := False;
            OnLowerEdge := False;
          end;
        PipeBL: // End of an edge segment
          begin
            if OnUpperEdge then Inc(Crossings); // L--7 counts as crossing
            OnUpperEdge := False;
            OnLowerEdge := False;
          end;
      end;
    end else
    begin
        // If we encounter ground while tracking an edge, reset edge tracking
        OnUpperEdge := False;
        OnLowerEdge := False;
    end;
  end;

  // Point is inside if the number of crossings is odd
  Result := (Crossings mod 2) = 1;
end;

// --- Main Program ---
var
  Path: TPath;
  StartConnections: TPipeConnections;
  Y, X: Integer;
  InsideCount: Integer;

begin
  ReadInputAndBuildGrid('input.txt');

  if (StartCoord.X = -1) or (MaxX = 0) or (MaxY = 0) then
  begin
    WriteLn('Error: Could not find Start S or grid is empty.');
    Halt(1);
  end;

  StartConnections := DetermineStartPipe(StartCoord);
  Path := FindPath(StartCoord, StartConnections);
  MarkPathAndReplaceStart(Path, StartConnections);

  InsideCount := 0;
  for Y := 0 to MaxY - 1 do
  begin
    for X := 0 to MaxX - 1 do
    begin
      if IsInside(CreateCoord(X, Y)) then
      begin
        Inc(InsideCount);
      end;
    end;
  end;

  WriteLn(InsideCount);
end.
