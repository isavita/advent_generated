
-- Standard library imports
with Ada.Text_IO; -- For file input/output and console output

-- The main procedure for the program.
-- In Ada, the main entry point is typically a parameterless procedure.
procedure Solve_Day5 is

   -- Constant for the input file name.
   Input_File_Name : constant String := "input.txt";

   -- Variable to store the count of nice strings found.
   Nice_Strings_Count : Natural := 0;

   -- File handle for the input file.
   Input_File : Ada.Text_IO.File_Type;

   -- Buffer for reading lines from the file.
   -- Assuming a maximum line length of 256 characters. This is a common safe
   -- assumption for Advent of Code inputs, which are typically not excessively long.
   Current_Line_Buffer : String (1 .. 256);
   -- Variable to store the actual length of the line read into the buffer.
   Line_Length : Natural;

   -- Function to check the first property: at least three vowels.
   -- Vowels are defined as 'a', 'e', 'i', 'o', 'u'.
   function Has_Three_Vowels (S : in String) return Boolean is
      Vowel_Count : Natural := 0;
   begin
      for I in S'Range loop -- Iterate over each character in the string S
         case S(I) is
            when 'a' | 'e' | 'i' | 'o' | 'u' =>
               Vowel_Count := Vowel_Count + 1;
            when others =>
               null; -- Character is not a vowel, do nothing.
         end case;
      end loop;
      return Vowel_Count >= 3;
   end Has_Three_Vowels;

   -- Function to check the second property: at least one letter appears twice in a row.
   function Has_Double_Letter (S : in String) return Boolean is
   begin
      -- A string must have at least two characters to contain a double letter.
      if S'Length < 2 then
         return False;
      end if;

      -- Iterate from the first character up to the second-to-last character.
      -- This allows checking S(I) and S(I+1) without going out of bounds.
      for I in S'First .. S'Last - 1 loop
         if S(I) = S(I+1) then
            return True; -- Found a double letter, no need to check further.
         end if;
      end loop;
      return False; -- No double letter found after checking all pairs.
   end Has_Double_Letter;

   -- Function to check the third property: does not contain specific disallowed substrings.
   -- Disallowed substrings are "ab", "cd", "pq", "xy".
   function Has_No_Disallowed_Substrings (S : in String) return Boolean is
   begin
      -- A string must have at least two characters to contain a 2-char substring.
      if S'Length < 2 then
         return True; -- Cannot contain disallowed 2-char substrings if too short.
      end if;

      -- Iterate from the first character up to the second-to-last character.
      for I in S'First .. S'Last - 1 loop
         -- Create a 2-character slice (a temporary string) for the current pair.
         declare
            Pair : constant String (1 .. 2) := (1 => S(I), 2 => S(I+1));
         begin
            -- Check if the current pair matches any of the disallowed substrings.
            if Pair = "ab" or else Pair = "cd" or else Pair = "pq" or else Pair = "xy" then
               return False; -- Found a disallowed substring, no need to check further.
            end if;
         end;
      end loop;
      return True; -- No disallowed substrings found after checking all pairs.
   end Has_No_Disallowed_Substrings;

   -- Function to determine if a string is "nice" based on all three properties.
   -- Uses 'and then' for short-circuit evaluation: if an earlier check fails,
   -- subsequent (potentially more expensive) checks are skipped.
   function Is_Nice (S : in String) return Boolean is
   begin
      return Has_Three_Vowels(S) and then
             Has_Double_Letter(S) and then
             Has_No_Disallowed_Substrings(S);
   end Is_Nice;

begin
   -- Open the input file for reading.
   -- If the file does not exist, a Name_Error exception will be raised.
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

   -- Loop through the file line by line until the end of the file is reached.
   loop
      exit when Ada.Text_IO.End_Of_File (Input_File);

      -- Read a line from the file into the buffer.
      -- 'Last' will be set to the actual length of the line read.
      Ada.Text_IO.Get_Line (File => Input_File, Item => Current_Line_Buffer, Last => Line_Length);
      
      -- Process the line. We pass a slice of the buffer to 'Is_Nice'
      -- to ensure we only work with the actual characters of the line.
      if Is_Nice (Current_Line_Buffer (1 .. Line_Length)) then
         Nice_Strings_Count := Nice_Strings_Count + 1;
      end if;
   end loop;

   -- Close the input file.
   Ada.Text_IO.Close (Input_File);

   -- Print the final count of nice strings to standard output.
   Ada.Text_IO.Put_Line ("Number of nice strings: " & Natural'Image (Nice_Strings_Count));

exception
   -- Handle the case where the input file does not exist.
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Error: File '" & Input_File_Name & "' not found.");
      Ada.Text_IO.Put_Line ("Please ensure 'input.txt' is in the same directory as the executable.");
   -- Handle any other unexpected errors during program execution.
   when others =>
      Ada.Text_IO.Put_Line ("An unexpected error occurred.");

end Solve_Day5;
