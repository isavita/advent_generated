
program DiskDefragmentation;

uses SysUtils;

const
  SUFFIX: array[0..4] of Integer = (17, 31, 73, 47, 23);

type
  TList = array[0..255] of Integer;
  TDense = array[0..15] of Byte;

function PopCount(b: Byte): Integer;
var
  x, c: Integer;
begin
  x := Integer(b);
  c := 0;
  while x > 0 do
  begin
    if (x and 1) = 1 then Inc(c);
    x := x shr 1;
  end;
  PopCount := c;
end;

procedure KnotHash(const input: string; var dense: TDense);
var
  lst: TList;
  lengths: array[0..255] of Integer;
  lengthsN: Integer;
  i, j, k, len, half: Integer;
  pos, skip: Integer;
  round: Integer;
  a, b, tmp: Integer;
  block, i2: Integer;
  val: Integer;
begin
  { Initialize the list 0..255 }
  for i := 0 to 255 do lst[i] := i;

  { Build lengths: ASCII codes of input + suffix }
  lengthsN := 0;
  for i := 1 to Length(input) do
  begin
    lengths[lengthsN] := Ord(input[i]);
    Inc(lengthsN);
  end;
  for i := 0 to 4 do
  begin
    lengths[lengthsN] := SUFFIX[i];
    Inc(lengthsN);
  end;

  pos := 0;
  skip := 0;

  { 64 rounds of processing }
  for round := 1 to 64 do
    for k := 0 to lengthsN - 1 do
    begin
      len := lengths[k];
      half := len div 2;
      for j := 0 to half - 1 do
      begin
        a := (pos + j) mod 256;
        b := (pos + len - 1 - j) mod 256;
        tmp := lst[a];
        lst[a] := lst[b];
        lst[b] := tmp;
      end;
      pos := (pos + len + skip) mod 256;
      Inc(skip);
    end;

  { Dense hash: XOR blocks of 16 }
  for block := 0 to 15 do
  begin
    val := 0;
    for i2 := 0 to 15 do
      val := val xor lst[block * 16 + i2];
    dense[block] := Byte(val);
  end;
end;

function RowBits(const dense: TDense): Integer;
var
  i, c: Integer;
begin
  c := 0;
  for i := 0 to 15 do
    inc(c, PopCount(dense[i]));
  RowBits := c;
end;

var
  f: Text;
  key: string;
  row: Integer;
  srow: string;
  dense: TDense;
  total: Integer;

begin
  { Read key from input.txt; assume one line with the key }
  Assign(f, 'input.txt');
  Reset(f);
  ReadLn(f, key);
  Close(f);

  total := 0;
  for row := 0 to 127 do
  begin
    srow := key + '-' + IntToStr(row);
    KnotHash(srow, dense);
    total := total + RowBits(dense);
  end;

  WriteLn(total);
end.
