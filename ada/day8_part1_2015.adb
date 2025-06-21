
-- Standard Ada libraries for I/O operations.
with Ada.Text_IO;

-- Main program entry point.
procedure Solve_Day8 is

   -- Function to calculate the "in-memory" length of a string literal.
   -- It parses the string according to the problem's escape rules.
   --
   -- Parameters:
   --   S : The string literal as read from the file (including its outer double quotes).
   --
   -- Returns:
   --   The number of characters the string represents in memory after
   --   interpreting escape sequences.
   function Calculate_Memory_Length(S : String) return Natural is
      Memory_Count : Natural := 0;
      -- Start parsing from the character immediately after the opening double quote.
      -- Ada string indices are typically 1-based. S'First is the index of the first character.
      Current_Index : Natural := S'First + 1;
   begin
      -- Iterate through the string, stopping before the closing double quote.
      -- The loop condition `Current_Index < S'Last` ensures we do not process
      -- the final closing quote itself, as it's not part of the in-memory string.
      while Current_Index < S'Last loop
         -- Check if the current character is the start of an escape sequence.
         if S(Current_Index) = '\' then
            -- It's a backslash, so it's an escape sequence.
            -- Look at the character immediately following the backslash to determine the type of escape.
            case S(Current_Index + 1) is
               when '\' | '"' =>
                  -- This handles '\\' (escaped backslash) and '\"' (escaped double quote).
                  -- Both represent a single character in memory.
                  Memory_Count := Memory_Count + 1;
                  -- Advance the index past both the backslash and the escaped character.
                  Current_Index := Current_Index + 2;
               when 'x' =>
                  -- This handles '\xHH' (hexadecimal escape).
                  -- This also represents a single character in memory.
                  Memory_Count := Memory_Count + 1;
                  -- Advance the index past '\', 'x', and the two hexadecimal digits.
                  Current_Index := Current_Index + 4;
               when others =>
                  -- This 'when others' clause should ideally not be reached with valid input
                  -- as per the problem description (only \\, \", \xHH are used).
                  -- If it were, it would indicate malformed input or an unhandled escape type.
                  -- For Advent of Code, input is typically guaranteed to be valid.
                  null; -- Do nothing, effectively skipping the invalid escape.
                        -- A more robust solution for general parsing might raise an exception here.
            end case;
         else
            -- It's a regular character (not a backslash, so not an escape sequence start).
            Memory_Count := Memory_Count + 1;
            -- Advance the index past this single character.
            Current_Index := Current_Index + 1;
         end if;
      end loop;

      return Memory_Count;
   end Calculate_Memory_Length;

   -- Declare variables for file handling and to accumulate total character counts.
   Input_File       : Ada.Text_IO.File_Type;
   -- A buffer to read lines into. 256 characters should be sufficient for typical AoC inputs.
   Line_Buffer      : String(1 .. 256);
   -- Stores the actual length of the line read into Line_Buffer.
   Actual_Line_Len  : Natural;
   Total_Code_Chars : Natural := 0; -- Accumulator for characters in code representation.
   Total_Mem_Chars  : Natural := 0; -- Accumulator for characters in in-memory representation.

begin
   -- Open the input file in read mode.
   Ada.Text_IO.Open(File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   -- Loop to read the file line by line until the end of the file is reached.
   while not Ada.Text_IO.End_Of_File(Input_File) loop
      -- Read a line from the file into Line_Buffer.
      -- Actual_Line_Len will be set to the true length of the line read.
      Ada.Text_IO.Get_Line(File => Input_File, Item => Line_Buffer, Last => Actual_Line_Len);

      -- Create a constant string slice that represents the actual line read.
      -- This ensures 'Current_Line'Length' is the exact length of the line from the file.
      declare
         Current_Line : constant String := Line_Buffer(1 .. Actual_Line_Len);
      begin
         -- Add the length of the current line (code representation) to the total.
         Total_Code_Chars := Total_Code_Chars + Current_Line'Length;

         -- Calculate the memory length for the current line and add it to the total.
         Total_Mem_Chars  := Total_Mem_Chars + Calculate_Memory_Length(Current_Line);
      end; -- End of the declare block for Current_Line.
   end loop;

   -- Close the input file after processing all lines.
   Ada.Text_IO.Close(Input_File);

   -- Print the final results to standard output.
   Ada.Text_IO.Put_Line("Total characters of code: " & Natural'Image(Total_Code_Chars));
   Ada.Text_IO.Put_Line("Total characters in memory: " & Natural'Image(Total_Mem_Chars));
   Ada.Text_IO.Put_Line("Difference (Code - Memory): " & Natural'Image(Total_Code_Chars - Total_Mem_Chars));

end Solve_Day8;
