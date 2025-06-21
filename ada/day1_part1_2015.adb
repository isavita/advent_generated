
with Ada.Text_IO;         -- For file operations (Open, Get, Close, Put_Line)
with Ada.Integer_Text_IO; -- For printing integer values

procedure Solve_Day1_Part1 is
   -- Define the name of the input file
   Input_File_Name : constant String := "input.txt";
   -- Declare a file handle
   Input_File      : Ada.Text_IO.File_Type;
   -- Variable to store the current character read from the file
   Current_Char    : Character;
   -- Variable to track Santa's current floor, initialized to 0
   Floor           : Integer := 0;
begin
   -- Open the input file for reading.
   -- If the file does not exist, a Name_Error exception will be raised.
   Ada.Text_IO.Open(File => Input_File, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

   -- Loop through the file character by character until the end of the file is reached.
   while not Ada.Text_IO.End_Of_File(Input_File) loop
      -- Read one character from the file.
      Ada.Text_IO.Get(File => Input_File, Item => Current_Char);

      -- Update the floor based on the character.
      if Current_Char = '(' then
         Floor := Floor + 1; -- Go up one floor
      elsif Current_Char = ')' then
         Floor := Floor - 1; -- Go down one floor
      -- Any other characters are ignored as per problem context (usually only '(' and ')' are present)
      end if;
   end loop;

   -- Close the input file after processing all characters.
   Ada.Text_IO.Close(Input_File);

   -- Print the final calculated floor to standard output.
   Ada.Text_IO.Put("Santa's final floor: ");
   Ada.Integer_Text_IO.Put(Floor);
   Ada.Text_IO.New_Line;

exception
   -- Handle the case where the input file is not found.
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line("Error: Input file '" & Input_File_Name & "' not found.");
   -- Handle any other unexpected errors during execution.
   when others =>
      Ada.Text_IO.Put_Line("An unexpected error occurred.");
end Solve_Day1_Part1;
