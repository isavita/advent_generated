program Day12;

var
  inputFile: Text;
  action: Char;
  value: Integer;
  shipX, shipY, wayX, wayY, tempX, tempY: Integer;

begin
  assign(inputFile, 'input.txt');
  reset(inputFile);

  shipX := 0;
  shipY := 0;
  wayX := 10;
  wayY := 1;

  while not eof(inputFile) do
  begin
    readln(inputFile, action, value);
    case action of
      'N': wayY := wayY + value;
      'S': wayY := wayY - value;
      'E': wayX := wayX + value;
      'W': wayX := wayX - value;
      'L': 
        begin
          for value := value div 90 downto 1 do
          begin
            tempX := wayX;
            wayX := -wayY;
            wayY := tempX;
          end;
        end;
      'R': 
        begin
          for value := value div 90 downto 1 do
          begin
            tempX := wayX;
            wayX := wayY;
            wayY := -tempX;
          end;
        end;
      'F': 
        begin
          shipX := shipX + value * wayX;
          shipY := shipY + value * wayY;
        end;
    end;
  end;

  writeln('Manhattan distance: ', abs(shipX) + abs(shipY));

  close(inputFile);
end.