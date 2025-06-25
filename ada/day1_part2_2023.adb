
-- This Ada program solves the "Day 1: Trebuchet?!" challenge from Advent of Code.
-- It reads calibration data from a file named "input.txt", calculates a value
-- for each line, and prints the total sum to standard output.
--
-- The program correctly handles both numeric digits ('1'-'9') and spelled-out
-- digits ("one" through "nine"), including cases where they overlap.

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Characters.Handling;

procedure Trebuchet_Calibration is

   -- Instantiate a generic I/O package for the Natural type for clean output.
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   -- An array of spelled-out digit strings.
   -- Using access types allows for an array of constants with different lengths
   -- without padding, which is both clean and memory-efficient.
   type String_Access is access constant String;
   Spelled_Digits : constant array (1 .. 9) of String_Access :=
     (new String'("one"),   new String'("two"),   new String'("three"),
      new String'("four"),  new String'("five"),  new String'("six"),
      new String'("seven"), new String'("eight"), new String'("nine"));

   -- Checks a specific position in a line for a digit, either numeric or spelled out.
   -- Returns the digit's value (1-9) or 0 if no digit is found starting at that index.
   function Get_Digit_At (Line : String; Index : Positive) return Natural is
   begin
      -- Check for a numeric digit first ('1'-'9').
      -- The problem does not specify '0' as a valid digit.
      if Line (Index) in '1' .. '9' then
         return Character'Pos (Line (Index)) - Character'Pos ('0');
      end if;

      -- Check for spelled-out digits starting at the current index.
      for J in Spelled_Digits'Range loop
         declare
            Word : constant String := Spelled_Digits (J).all;
         begin
            -- Ensure the slice is within the bounds of the Line string.
            if Index + Word'Length - 1 <= Line'Last then
               if Line (Index .. Index + Word'Length - 1) = Word then
                  return J; -- The array index (1-9) is the digit's value.
               end if;
            end if;
         end;
      end loop;

      return 0; -- No digit found starting at this position.
   end Get_Digit_At;


   File_Name  : constant String := "input.txt";
   Input_File : Ada.Text_IO.File_Type;
   Total_Sum  : Natural := 0;

begin -- Main procedure execution

   -- Open the input file for reading.
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => File_Name);

   -- Process the file line by line until the end is reached.
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (Input_File);
         First_Digit : Natural := 0;
         Last_Digit  : Natural := 0;
      begin
         if Line'Length > 0 then
            -- Find the first and last digit by iterating through the line.
            -- This single pass correctly identifies the first and last occurrences
            -- of any digit, handling overlaps like "oneight" correctly.
            for I in Line'Range loop
               declare
                  Digit_Value : constant Natural := Get_Digit_At (Line, I);
               begin
                  if Digit_Value > 0 then
                     if First_Digit = 0 then -- This is the first digit we've found.
                        First_Digit := Digit_Value;
                     end if;
                     Last_Digit := Digit_Value; -- Always update with the most recent digit.
                  end if;
               end;
            end loop;

            -- If at least one digit was found, calculate the two-digit calibration
            -- value and add it to the total sum.
            if First_Digit > 0 then
               Total_Sum := Total_Sum + (First_Digit * 10 + Last_Digit);
            end if;
         end if;
      end;
   end loop;

   -- Close the file handle once processing is complete.
   Ada.Text_IO.Close (Input_File);

   -- Print the final result to standard output.
   Ada.Text_IO.Put ("The sum of all of the calibration values is: ");
   Natural_IO.Put (Item => Total_Sum, Width => 1);
   Ada.Text_IO.New_Line;

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: Input file '" & File_Name & "' not found.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "An unexpected error occurred.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Trebuchet_Calibration;
