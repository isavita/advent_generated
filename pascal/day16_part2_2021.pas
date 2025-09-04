program PacketDecoder;

{$mode objfpc}{$H+}
uses
  SysUtils;

function hex_to_bin(hex: string): string;
var
  i: Integer;
  c: Char;
  bin: string;
begin
  hex := UpperCase(Trim(hex));
  bin := '';
  for i := 1 to Length(hex) do begin
    c := hex[i];
    case c of
      '0': bin := bin + '0000';
      '1': bin := bin + '0001';
      '2': bin := bin + '0010';
      '3': bin := bin + '0011';
      '4': bin := bin + '0100';
      '5': bin := bin + '0101';
      '6': bin := bin + '0110';
      '7': bin := bin + '0111';
      '8': bin := bin + '1000';
      '9': bin := bin + '1001';
      'A': bin := bin + '1010';
      'B': bin := bin + '1011';
      'C': bin := bin + '1100';
      'D': bin := bin + '1101';
      'E': bin := bin + '1110';
      'F': bin := bin + '1111';
    end;
  end;
  hex_to_bin := bin;
end;

function bits_to_int(bin: string; pos: Integer; len: Integer): Int64;
var
  i: Integer;
  val: Int64;
begin
  val := 0;
  for i := 0 to len - 1 do
    val := (val shl 1) or (Ord(bin[pos + i]) - Ord('0'));
  bits_to_int := val;
end;

procedure parse_packet(bin: string; var idx: Integer; var version: Integer; var value: Int64);
var
  type_id: Integer;
  length_type_id: Integer;
  sub_packet_length: Integer;
  num_sub_packets: Integer;
  i: Integer;
  prev_idx: Integer;
  sub_ver: Integer;
  sub_val: Int64;
  values: array of Int64;
begin
  version := Integer(bits_to_int(bin, idx, 3));
  type_id := Integer(bits_to_int(bin, idx + 3, 3));
  idx := idx + 6;

  if type_id = 4 then begin
    value := 0;
    while bin[idx] = '1' do begin
      value := (value shl 4) or bits_to_int(bin, idx + 1, 4);
      idx := idx + 5;
    end;
    value := (value shl 4) or bits_to_int(bin, idx + 1, 4);
    idx := idx + 5;
    Exit;
  end;

  length_type_id := Ord(bin[idx]) - Ord('0');
  idx := idx + 1;

  if length_type_id = 0 then begin
    sub_packet_length := Integer(bits_to_int(bin, idx, 15));
    idx := idx + 15;
    num_sub_packets := 0; // not used directly
  end
  else begin
    num_sub_packets := Integer(bits_to_int(bin, idx, 11));
    idx := idx + 11;
    sub_packet_length := 0; // not used directly
  end;

  SetLength(values, 0);

  while true do begin
    if (length_type_id = 0) and (sub_packet_length = 0) then Break;
    if (length_type_id = 1) and (num_sub_packets = 0) then Break;

    prev_idx := idx;
    parse_packet(bin, idx, sub_ver, sub_val);
    SetLength(values, Length(values) + 1);
    values[High(values)] := sub_val;

    if length_type_id = 0 then
      sub_packet_length := sub_packet_length - (idx - prev_idx)
    else
      Dec(num_sub_packets);
  end;

  case type_id of
    0: begin
         value := 0;
         for i := 0 to High(values) do value := value + values[i];
       end;
    1: begin
         value := 1;
         for i := 0 to High(values) do value := value * values[i];
       end;
    2: begin
         value := values[0];
         for i := 1 to High(values) do if values[i] < value then value := values[i];
       end;
    3: begin
         value := values[0];
         for i := 1 to High(values) do if values[i] > value then value := values[i];
       end;
    5: begin
         value := 0;
         if (Length(values) > 1) and (values[0] > values[1]) then value := 1;
       end;
    6: begin
         value := 0;
         if (Length(values) > 1) and (values[0] < values[1]) then value := 1;
       end;
    7: begin
         value := 0;
         if (Length(values) > 1) and (values[0] = values[1]) then value := 1;
       end;
  else
     value := 0;
  end;
end;

var
  fIn: Text;
  hex_str: string;
  bin_str: string;
  idx: Integer;
  ver: Integer;
  ans: Int64;
begin
  AssignFile(fIn, 'input.txt');
  Reset(fIn);
  ReadLn(fIn, hex_str);
  CloseFile(fIn);

  bin_str := hex_to_bin(hex_str);
  idx := 1;
  ver := 0;
  parse_packet(bin_str, idx, ver, ans);
  WriteLn(ans);
end.