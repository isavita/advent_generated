
with Ada.Text_IO;

procedure Solve_Matchsticks is

   -- Maximum expected line length. Advent of Code inputs usually fit within a few hundred characters.
   -- If lines could be arbitrarily long, Ada.Strings.Unbounded would be more robust.
   Max_Line_Length : constant Natural := 256;
   Line_Buffer     : String (1 .. Max_Line_Length);
   Actual_Line_Len : Natural; -- Stores the actual length of the line read by Get_Line

   Total_Code_Chars    : Natural := 0;
   Total_Memory_Chars  : Natural := 0;
   Total_Encoded_Chars : Natural := 0;

   Input_File : Ada.Text_IO.File_Type;

   -----------------------------------------------------------------------------
   -- Function to calculate the in-memory length of a string literal (Part 1)
   -- S: The string literal as it appears in the code (e.g., "abc", "aaa\"aaa")
   -- Returns: The number of characters the string represents in memory.
   -----------------------------------------------------------------------------
   function Calculate_In_Memory_Length (S : String) return Natural is
      In_Memory_Len : Natural := 0;
      I             : Positive := S'First;
   begin
      -- Skip the initial double quote.
      -- The loop will process characters between the outer quotes.
      I := I + 1;

      -- Iterate until the character before the final double quote.
      while I < S'Last loop
         if S(I) = '\' then
            -- Handle escape sequences
            if S(I+1) = '\' or S(I+1) = '"' then
               -- Escaped backslash (\\) or double quote (\")
               In_Memory_Len := In_Memory_Len + 1;
               I := I + 2; -- Advance past '\' and the escaped character
            elsif S(I+1) = 'x' then
               -- Hexadecimal escape (\xHH)
               In_Memory_Len := In_Memory_Len + 1;
               I := I + 4; -- Advance past '\', 'x', and two hex digits
            else
               -- This case should not occur based on the problem description,
               -- but for robustness, treat an unrecognized escape as a literal backslash.
               In_Memory_Len := In_Memory_Len + 1;
               I := I + 1;
            end if;
         else
            -- Regular character
            In_Memory_Len := In_Memory_Len + 1;
            I := I + 1; -- Advance past the current character
         end if;
      end loop;

      return In_Memory_Len;
   end Calculate_In_Memory_Length;

   -----------------------------------------------------------------------------
   -- Function to calculate the length of the newly encoded string (Part 2)
   -- S: The original string literal as it appears in the code.
   -- Returns: The number of characters in the new encoded representation.
   -----------------------------------------------------------------------------
   function Calculate_Encoded_Length (S : String) return Natural is
      -- Start with 2 for the new surrounding double quotes.
      Encoded_Len : Natural := 2;
   begin
      -- Iterate over every character of the original string literal,
      -- including its own surrounding quotes.
      for I in S'Range loop
         declare
            C : Character := S(I);
         begin
            if C = '"' or C = '\' then
               -- Double quotes and backslashes need to be escaped.
               -- Each will become two characters (e.g., '"' -> '\"').
               Encoded_Len := Encoded_Len + 2;
            else
               -- All other characters are copied as-is.
               Encoded_Len := Encoded_Len + 1;
            end if;
         end;
      end loop;
      return Encoded_Len;
   end Calculate_Encoded_Length;

begin
   -- Open the input file
   Ada.Text_IO.Open (File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => "input.txt");

   -- Process each line in the file
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      Ada.Text_IO.Get_Line (File => Input_File, Item => Line_Buffer, Last => Actual_Line_Len);

      -- Create a slice for the actual string content read
      declare
         Current_Line : constant String := Line_Buffer(1 .. Actual_Line_Len);
      begin
         -- Part 1 calculations
         Total_Code_Chars    := Total_Code_Chars + Current_Line'Length;
         Total_Memory_Chars  := Total_Memory_Chars + Calculate_In_Memory_Length(Current_Line);

         -- Part 2 calculations
         Total_Encoded_Chars := Total_Encoded_Chars + Calculate_Encoded_Length(Current_Line);
      end;
   end loop;

   -- Close the input file
   Ada.Text_IO.Close (Input_File);

   -- Print results for Part 1
   Ada.Text_IO.Put_Line ("Part 1: " & Natural'Image (Total_Code_Chars - Total_Memory_Chars));

   -- Print results for Part 2
   Ada.Text_IO.Put_Line ("Part 2: " & Natural'Image (Total_Encoded_Chars - Total_Code_Chars));

end Solve_Matchsticks;
