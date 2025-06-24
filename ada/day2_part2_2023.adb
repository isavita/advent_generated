
-- main.adb
--
-- Solves the Advent of Code 2023, Day 2: Cube Conundrum challenge.
-- Reads game data from "input.txt" and calculates two results:
--
-- Part 1: The sum of IDs of games possible with a specific number of cubes.
-- Part 2: The sum of the "power" of the minimum set of cubes for each game.
--
-- The program uses an efficient single-pass, cursor-based parsing method
-- to process the input file without creating unnecessary intermediate strings.

with Ada.Text_IO;
with Ada.Command_Line;

procedure Main is

   -- Define a type for the colors to make the code more readable and robust.
   type Color_Type is (Red, Green, Blue);

   -- Use an array indexed by Color_Type to store cube counts.
   type Cube_Counts is array (Color_Type) of Natural;

   -- The maximum number of cubes allowed for a game to be "possible" in Part 1.
   Max_Cubes : constant Cube_Counts := (Red => 12, Green => 13, Blue => 14);

   -- Input file name.
   Input_File_Name : constant String := "input.txt";

   -- File handle.
   Input_File : Ada.Text_IO.File_Type;

   -- Accumulators for the final answers.
   Sum_Of_Possible_IDs : Natural := 0;
   -- The power can exceed Natural'Last, so use a larger integer type for safety.
   Sum_Of_Powers : Long_Long_Integer := 0;

begin
   -- Open the input file for reading.
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);

   -- Process the file line by line.
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);

         -- Variables for the current game being processed.
         Game_ID               : Natural;
         Min_Required_For_Game : Cube_Counts := (others => 0);
         Game_Is_Possible      : Boolean := True;

         -- A cursor to keep track of our position while parsing the line.
         -- This is more efficient than creating many substrings.
         Cursor : Positive := Line'First;
      begin
         -- The line format is "Game <ID>: <set 1>; <set 2>; ..."
         -- Example: "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

         -- 1. Parse the Game ID.
         Cursor := Cursor + 5; -- Skip "Game "
         declare
            Start_ID : constant Positive := Cursor;
         begin
            while Line (Cursor) /= ':' loop
               Cursor := Cursor + 1;
            end loop;
            Game_ID := Natural'Value (Line (Start_ID .. Cursor - 1));
         end;
         Cursor := Cursor + 2; -- Skip ": "

         -- 2. Loop through all cube reveals in the game.
         loop
            -- Each reveal is of the form "<count> <color>"
            declare
               Count : Natural;
               Color : Color_Type;
            begin
               -- Parse the count.
               declare
                  Start_Count : constant Positive := Cursor;
               begin
                  while Line (Cursor) in '0' .. '9' loop
                     Cursor := Cursor + 1;
                  end loop;
                  Count := Natural'Value (Line (Start_Count .. Cursor - 1));
               end;
               Cursor := Cursor + 1; -- Skip the space after the count.

               -- Parse the color. We can identify the color by its first letter.
               case Line (Cursor) is
                  when 'r' =>
                     Color  := Red;
                     Cursor := Cursor + 3; -- "red"
                  when 'g' =>
                     Color  := Green;
                     Cursor := Cursor + 5; -- "green"
                  when 'b' =>
                     Color  := Blue;
                     Cursor := Cursor + 4; -- "blue"
                  when others =>
                     raise Program_Error with "Invalid color found in input.";
               end case;

               -- --- Part 1 Logic ---
               -- Check if this reveal makes the game impossible.
               if Count > Max_Cubes (Color) then
                  Game_Is_Possible := False;
               end if;

               -- --- Part 2 Logic ---
               -- Update the minimum number of cubes required for this game by
               -- tracking the maximum number of cubes of each color seen.
               if Count > Min_Required_For_Game (Color) then
                  Min_Required_For_Game (Color) := Count;
               end if;
            end;

            -- Exit the loop if we've reached the end of the line.
            exit when Cursor > Line'Last;

            -- Skip the delimiter (", " or "; ") to get to the next reveal.
            Cursor := Cursor + 2;
         end loop;

         -- 3. After processing all reveals for the game, update the totals.

         -- Part 1: Add the ID if the game was possible.
         if Game_Is_Possible then
            Sum_Of_Possible_IDs := Sum_Of_Possible_IDs + Game_ID;
         end if;

         -- Part 2: Calculate the power of the minimum set and add it to the sum.
         declare
            Power : constant Long_Long_Integer :=
              Long_Long_Integer (Min_Required_For_Game (Red)) *
              Long_Long_Integer (Min_Required_For_Game (Green)) *
              Long_Long_Integer (Min_Required_For_Game (Blue));
         begin
            Sum_Of_Powers := Sum_Of_Powers + Power;
         end;

      end; -- End of the declare block for a single line.
   end loop;

   Ada.Text_IO.Close (Input_File);

   -- Print the final results to standard output.
   Ada.Text_IO.Put_Line ("--- Day 2: Cube Conundrum ---");
   Ada.Text_IO.Put_Line ("Part 1 Result:" & Natural'Image (Sum_Of_Possible_IDs));
   Ada.Text_IO.Put_Line ("Part 2 Result:" & Long_Long_Integer'Image (Sum_Of_Powers));

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: Input file '" & Input_File_Name & "' not found.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when Constraint_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: A data constraint was violated. Check input file format.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   when others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "An unexpected error occurred during processing.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

end Main;
