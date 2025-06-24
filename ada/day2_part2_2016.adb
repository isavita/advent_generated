
------------------------------------------------------------------
--
--  Solution for Advent of Code - Day 2: Bathroom Security
--
--  This program reads a series of movement instructions from a
--  file named "input.txt". It calculates two bathroom codes
--  based on two different keypad layouts.
--
--  - Part 1 uses a standard 3x3 numeric keypad.
--  - Part 2 uses a diamond-shaped keypad.
--
--  The program processes the input file once, calculating both
--  codes simultaneously for efficiency. The final codes are
--  printed to standard output.
--
------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

procedure Bathroom_Security is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Command_Line;

   -- Part 1: Standard 3x3 keypad
   -- Coordinates are (Row, Column), starting at (0, 0) for '1'.
   -- 1 2 3
   -- 4 5 6
   -- 7 8 9
   type Keypad_3x3 is array (0 .. 2, 0 .. 2) of Character;
   Keypad_Part_1 : constant Keypad_3x3 :=
     (('1', '2', '3'),
      ('4', '5', '6'),
      ('7', '8', '9'));

   -- Part 2: Diamond-shaped keypad
   -- Embedded in a 5x5 grid, with ' ' for invalid positions.
   --     1
   --   2 3 4
   -- 5 6 7 8 9
   --   A B C
   --     D
   type Keypad_5x5 is array (0 .. 4, 0 .. 4) of Character;
   Keypad_Part_2 : constant Keypad_5x5 :=
     ((' ', ' ', '1', ' ', ' '),
      (' ', '2', '3', '4', ' '),
      ('5', '6', '7', '8', '9'),
      (' ', 'A', 'B', 'C', ' '),
      (' ', ' ', 'D', ' ', ' '));

   -- File handling
   Input_File : File_Type;
   File_Name  : constant String := "input.txt";

   -- State variables for tracking position and building the codes
   Row_1 : Integer := 1; -- Start at '5' -> (1, 1)
   Col_1 : Integer := 1;
   Code_1 : Unbounded_String;

   Row_2 : Integer := 2; -- Start at '5' -> (2, 0)
   Col_2 : Integer := 0;
   Code_2 : Unbounded_String;

begin
   -- Open the input file for reading
   Open (File => Input_File, Mode => In_File, Name => File_Name);

   -- Process the file line by line
   while not End_Of_File (Input_File) loop
      declare
         Line : constant String := Get_Line (Input_File);
      begin
         -- Process each move instruction in the current line
         for Move of Line loop

            -- Part 1: Move on the 3x3 keypad
            -- This logic uses Min/Max to concisely clamp coordinates within the 0..2 range.
            case Move is
               when 'U' => Row_1 := Integer'Max (Row_1 - 1, 0);
               when 'D' => Row_1 := Integer'Min (Row_1 + 1, 2);
               when 'L' => Col_1 := Integer'Max (Col_1 - 1, 0);
               when 'R' => Col_1 := Integer'Min (Col_1 + 1, 2);
               when others => null; -- Ignore any other characters
            end case;

            -- Part 2: Move on the diamond keypad
            declare
               -- Use temporary variables to calculate the potential next position
               New_Row_2 : Integer := Row_2;
               New_Col_2 : Integer := Col_2;
            begin
               case Move is
                  when 'U' => New_Row_2 := Row_2 - 1;
                  when 'D' => New_Row_2 := Row_2 + 1;
                  when 'L' => New_Col_2 := Col_2 - 1;
                  when 'R' => New_Col_2 := Col_2 + 1;
                  when others => null;
               end case;

               -- A move is valid only if it's within the 5x5 grid AND the
               -- target position is not an empty space.
               if New_Row_2 in Keypad_Part_2'Range (1) and then
                  New_Col_2 in Keypad_Part_2'Range (2) and then
                  Keypad_Part_2 (New_Row_2, New_Col_2) /= ' '
               then
                  -- If valid, update the current position
                  Row_2 := New_Row_2;
                  Col_2 := New_Col_2;
               end if;
            end;

         end loop;

         -- After processing all moves in a line, append the resulting button to the code
         Append (Source => Code_1, New_Item => Keypad_Part_1 (Row_1, Col_1));
         Append (Source => Code_2, New_Item => Keypad_Part_2 (Row_2, Col_2));
      end;
   end loop;

   -- Close the file once processing is complete
   Close (Input_File);

   -- Print the final results to standard output
   Put_Line ("--- Day 2: Bathroom Security ---");
   New_Line;
   Put_Line ("Part 1 Code (Standard Keypad): " & To_String (Code_1));
   Put_Line ("Part 2 Code (Diamond Keypad):  " & To_String (Code_2));
   New_Line;

exception
   -- Handle potential file-related errors gracefully
   when Status_Error =>
      Put_Line (Standard_Error, "Error: Could not open file '" & File_Name & "'. Check permissions.");
      Set_Exit_Status (Failure);
   when Name_Error =>
      Put_Line (Standard_Error, "Error: File '" & File_Name & "' not found.");
      Set_Exit_Status (Failure);
   when others =>
      Put_Line (Standard_Error, "An unexpected error occurred during file processing.");
      Set_Exit_Status (Failure);

end Bathroom_Security;
