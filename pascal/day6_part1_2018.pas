program solution;

uses SysUtils, Classes, Math;

type
	Point = record
		X, Y: Integer;
	end;

var
	fileInput: Text;
	line, coordX, coordY: String;
	points: array of Point;
	maxX, maxY, i, j, k, minDist, maxXDist, maxYDist, gridLength, maxArea: Integer;
	grid: array of array of Integer;
	areas: array of Integer;
	infinite: array of Boolean;

begin
	Assign(fileInput, 'input.txt');
	Reset(fileInput);
	maxX := 0;
	maxY := 0;
	SetLength(points, 0);
	while not Eof(fileInput) do
	begin
		Readln(fileInput, line);
		coordX := Copy(line, 1, Pos(', ', line) - 1);
		Delete(line, 1, Pos(', ', line) + 1);
		coordY := line;
		i := StrToInt(coordX);
		j := StrToInt(coordY);
		if i > maxX then
			maxX := i;
		if j > maxY then
			maxY := j;
		SetLength(points, Length(points) + 1);
		points[High(points)].X := i;
		points[High(points)].Y := j;
	end;
	Close(fileInput);

	SetLength(grid, maxX + 2);
	for i := Low(grid) to High(grid) do
	begin
		SetLength(grid[i], maxY + 2);
	end;
	gridLength := Length(grid);
	SetLength(areas, Length(points));
	SetLength(infinite, Length(points));
	for i := Low(grid) to High(grid) do
	begin
		for j := Low(grid[i]) to High(grid[i]) do
		begin
			minDist := maxX + maxY;
			for k := Low(points) to High(points) do
			begin
				maxXDist := abs(points[k].X - i);
				maxYDist := abs(points[k].Y - j);
				if maxXDist + maxYDist < minDist then
				begin
					minDist := maxXDist + maxYDist;
					grid[i][j] := k;
				end
				else if maxXDist + maxYDist = minDist then
				begin
					grid[i][j] := -1;
				end;
			end;
			if grid[i][j] <> -1 then
			begin
				if (i = 0) or (j = 0) or (i = maxX + 1) or (j = maxY + 1) then
					infinite[grid[i][j]] := True;
				areas[grid[i][j]] := areas[grid[i][j]] + 1;
			end;
		end;
	end;

	maxArea := 0;
	for i := Low(areas) to High(areas) do
	begin
		if (not infinite[i]) and (areas[i] > maxArea) then
			maxArea := areas[i];
	end;
	Writeln(maxArea);
end.