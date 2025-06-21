
with Ada.Text_IO;         -- For file input/output operations
with Ada.Integer_Text_IO; -- For printing integer values

-- The main procedure for the program. In Ada, the top-level procedure
-- serves as the entry point, similar to 'main' in C/C++.
procedure Solve_Day1 is
   -- Declare a file handle for the input file.
   Input_File : Ada.Text_IO.File_Type;
   
   -- Variable to store the current character read from the file.
   Current_Char : Character;
   
   -- Tracks Santa's current floor. Starts at floor 0.
   Current_Floor : Integer := 0;
   
   -- Tracks the 1-indexed position of the current character being processed.
   Character_Position : Integer := 0;
   
   -- Stores the 1-indexed position when Santa first enters the basement (floor -1).
   -- Initialized to 0, indicating it hasn't been found yet.
   Basement_Position : Integer := 0; 

begin
   -- --- Part 1 & Part 2 Logic ---

   -- Open the input file "input.txt" for reading.
   -- If the file does not exist, a Name_Error exception will be raised.
   Ada.Text_IO.Open(File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   
   -- Redirect standard input to read from Input_File.
   -- This allows using Ada.Text_IO.Get and Ada.Text_IO.End_Of_File directly on the file.
   Ada.Text_IO.Set_Input(Input_File);

   -- Loop through the file character by character until the end of the file is reached.
   while not Ada.Text_IO.End_Of_File loop
      -- Read the next character from the input file.
      Ada.Text_IO.Get(Current_Char);
      
      -- Increment the character position counter. Positions are 1-indexed.
      Character_Position := Character_Position + 1;

      -- Update Santa's floor based on the character.
      if Current_Char = '(' then
         Current_Floor := Current_Floor + 1; -- Go up one floor
      elsif Current_Char = ')' then
         Current_Floor := Current_Floor - 1; -- Go down one floor
      end if;

      -- Check for Part Two condition:
      -- If Basement_Position is still 0 (meaning the basement hasn't been entered yet)
      -- AND Santa's current floor is -1, then we've found the first time he entered the basement.
      if Basement_Position = 0 and Current_Floor = -1 then
         Basement_Position := Character_Position;
      end if;
   end loop;

   -- Close the input file after processing.
   Ada.Text_IO.Close(Input_File);

   -- --- Output Results ---

   -- Print the final floor for Part 1.
   -- Integer'Image converts an integer to its string representation.
   Ada.Text_IO.Put_Line("Part 1: Final floor: " & Integer'Image(Current_Floor));
   
   -- Print the position where Santa first entered the basement for Part 2.
   Ada.Text_IO.Put_Line("Part 2: First enters basement at position: " & Integer'Image(Basement_Position));

exception
   -- Handle the case where "input.txt" is not found.
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line("Error: input.txt not found. Please ensure the file exists in the same directory.");
   -- Catch any other unexpected errors during execution.
   when others =>
      Ada.Text_IO.Put_Line("An unexpected error occurred during program execution.");
end Solve_Day1;
