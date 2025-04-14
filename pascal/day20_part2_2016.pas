
program Solve;

{$mode objfpc}{$H+} // Use Free Pascal Compiler directives

uses
  SysUtils, Math; // SysUtils for StrToUInt, Math for Max (though Max is simple)

const
  MaxIP = 4294967295;
  // Adjust MaxRanges if input file can have more lines
  MaxRanges = 4000; 

type
  TRange = record
    start, stop: Cardinal; // Use Cardinal (unsigned 32-bit) for IP addresses
  end;
  TRangeArray = array[1..MaxRanges] of TRange;

var
  data: TRangeArray;
  count: Integer = 0;
  inputFile: Text;
  line: string;
  i: Integer;
  hyphenPos: Integer;
  startStr, endStr: string;
  nextAllowed: QWord;    // Use QWord (unsigned 64-bit) to avoid overflow with MaxIP + 1
  totalAllowed: QWord;   // Use QWord for the potentially large count
  rangeEndPlus1: QWord;
  tempRange: TRange; // For potential sorting/swapping if needed later

// --- QuickSort Implementation (needed because Pascal standard sort doesn't easily handle records) ---
procedure Swap(var a, b: TRange);
var temp: TRange;
begin
  temp := a;
  a := b;
  b := temp;
end;

// Partition function for QuickSort, comparing by start value
function Partition(var arr: TRangeArray; low, high: Integer): Integer;
var
  pivotValue: Cardinal;
  i, j: Integer;
begin
  pivotValue := arr[high].start; // Choose the last element as pivot
  i := low - 1; // Index of smaller element

  for j := low to high - 1 do
  begin
    // If current element's start is smaller than the pivot
    if arr[j].start < pivotValue then
    begin
      i := i + 1; // Increment index of smaller element
      Swap(arr[i], arr[j]);
    end;
  end;
  // Place the pivot element in its correct position
  Swap(arr[i + 1], arr[high]);
  Result := i + 1; // Return the partition index
end;

// Recursive QuickSort procedure
procedure QuickSort(var arr: TRangeArray; low, high: Integer);
var
  pi: Integer;
begin
  if low < high then
  begin
    pi := Partition(arr, low, high); // Get partitioning index
    QuickSort(arr, low, pi - 1);     // Sort elements before partition
    QuickSort(arr, pi + 1, high);    // Sort elements after partition
  end;
end;
// --- End QuickSort ---

begin
  Assign(inputFile, 'input.txt');
  try
    Reset(inputFile);
  except
    on E: EInOutError do begin
      WriteLn(ErrOutput, 'Error: Cannot open input file "input.txt". ', E.Message);
      Halt(1);
    end;
  end;

  // Read ranges from file
  while not Eof(inputFile) do
  begin
    Readln(inputFile, line);
    line := Trim(line); // Remove leading/trailing whitespace
    if line = '' then continue; // Skip empty lines

    Inc(count);
    if count > MaxRanges then
    begin
      WriteLn(ErrOutput, 'Error: Number of ranges exceeds MaxRanges limit (', MaxRanges, ').');
      Halt(1);
    end;

    hyphenPos := Pos('-', line);
    if hyphenPos <= 0 then
    begin
       WriteLn(ErrOutput, 'Error: Invalid format on line ', count, ': "', line, '"');
       Halt(1);
    end;

    startStr := Copy(line, 1, hyphenPos - 1);
    endStr := Copy(line, hyphenPos + 1, Length(line) - hyphenPos);

    try
      data[count].start := StrToUInt(Trim(startStr));
      data[count].stop := StrToUInt(Trim(endStr));
    except
      on E: Exception do begin
         WriteLn(ErrOutput, 'Error converting number on line ', count, ': "', line, '". ', E.Message);
         Halt(1);
       end;
    end;
     if data[count].start > data[count].stop then
     begin
        WriteLn(ErrOutput, 'Error: Range start > stop on line ', count, ': "', line, '"');
        Halt(1);
     end;
  end;
  Close(inputFile);

  if count = 0 then
  begin
     // If no blocked ranges, all IPs are allowed (0 to MaxIP inclusive)
     WriteLn(QWord(MaxIP) + 1);
     Halt(0);
  end;

  // Sort ranges by start IP
  QuickSort(data, 1, count);

  // Calculate allowed IPs using the merged range approach
  nextAllowed := 0;  // The next IP address that *could* be allowed
  totalAllowed := 0; // Accumulator for allowed IPs

  for i := 1 to count do
  begin
    // If there's a gap between the last blocked IP and the current range start
    if data[i].start > nextAllowed then
    begin
      totalAllowed := totalAllowed + (QWord(data[i].start) - nextAllowed);
    end;

    // Update the next potentially allowed IP address.
    // It's the maximum of the current nextAllowed and the end of the current range + 1.
    // Use QWord for calculation to handle end = MaxIP safely.
    if data[i].stop = MaxIP then
    begin
       // If this range covers up to MaxIP, no more IPs can be allowed
       nextAllowed := QWord(MaxIP) + 1;
       Break; // Optimization: can stop processing ranges
    end
    else
    begin
       rangeEndPlus1 := QWord(data[i].stop) + 1;
       if rangeEndPlus1 > nextAllowed then
       begin
         nextAllowed := rangeEndPlus1;
       end;
    end;

  end;

  // After checking all ranges, if nextAllowed is still within the valid IP range,
  // add the remaining allowed IPs from nextAllowed up to MaxIP.
  if nextAllowed <= MaxIP then
  begin
    totalAllowed := totalAllowed + (QWord(MaxIP) - nextAllowed + 1);
  end;

  // Print the final count
  WriteLn(totalAllowed);

end.
