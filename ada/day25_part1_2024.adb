
-- =============================================================================
--  Solution for Advent of Code: Day 25
--
--  Reads lock and key schematics from "input.txt", determines which pairs
--  are compatible, and prints the total count of compatible pairs to
--  standard output.
-- =============================================================================
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Exceptions;

procedure Main is

   -- Constants defining the structure of the schematics.
   Schematic_Width  : constant := 5;
   Schematic_Height : constant := 7;
   Pin_Space_Height : constant := 5; -- The number of rows where pins can exist.

   -- Custom types for clarity and type safety.
   subtype Column_Index is Natural range 0 .. Schematic_Width - 1;
   subtype Height is Natural range 0 .. Pin_Space_Height;
   type Pin_Heights is array (Column_Index) of Height;

   -- Instantiate generic Vector packages to store the parsed locks and keys.
   package Lock_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Pin_Heights);

   package Key_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Pin_Heights);

   -- A type to hold the 7x5 grid of characters for a single schematic.
   type Schematic_Data is
     array (1 .. Schematic_Height) of String (1 .. Schematic_Width);

   ---------------------------------------------------------------------------
   --  Fits
   --  Checks if a given key can fit into a lock without collision.
   --  A fit occurs if for every column, the sum of the lock's pin height
   --  and the key's height does not exceed the available space.
   ---------------------------------------------------------------------------
   function Fits (Lock : Pin_Heights; Key : Pin_Heights) return Boolean is
   begin
      for I in Pin_Heights'Range loop
         if Lock (I) + Key (I) > Pin_Space_Height then
            return False; -- A collision was found in this column.
         end if;
      end loop;
      return True; -- No collisions found, the pair fits.
   end Fits;

   ---------------------------------------------------------------------------
   --  Parse_Schematic
   --  Analyzes a 7x5 schematic to determine if it's a lock or a key and
   --  calculates the corresponding pin/key heights.
   ---------------------------------------------------------------------------
   procedure Parse_Schematic
     (Data    : in  Schematic_Data;
      Is_Lock : out Boolean;
      Is_Key  : out Boolean;
      Heights : out Pin_Heights)
   is
      Lock_Top_Row : constant String := (1 .. Schematic_Width => '#');
      Lock_Bot_Row : constant String := (1 .. Schematic_Width => '.');
      Key_Top_Row  : constant String := Lock_Bot_Row;
      Key_Bot_Row  : constant String := Lock_Top_Row;
   begin
      -- Determine type based on top and bottom rows.
      Is_Lock :=
        (Data (1) = Lock_Top_Row and then Data (Schematic_Height) = Lock_Bot_Row);
      Is_Key :=
        (Data (1) = Key_Top_Row and then Data (Schematic_Height) = Key_Bot_Row);

      -- If it's a valid lock or key, calculate the heights.
      if Is_Lock or Is_Key then
         Heights := (others => 0); -- Initialize heights to zero.
         for Col in Column_Index loop
            for Row in 2 .. Schematic_Height - 1 loop -- Iterate through pin space
               if Data (Row) (Col + 1) = '#' then
                  Heights (Col) := Heights (Col) + 1;
               end if;
            end loop;
         end loop;
      end if;
   end Parse_Schematic;

   -- Main program variables
   Input_File : Ada.Text_IO.File_Type;
   File_Name  : constant String := "input.txt";
   Locks      : Lock_Vectors.Vector;
   Keys       : Key_Vectors.Vector;

begin
   -- Open the input file, with error handling for a missing file.
   begin
      Ada.Text_IO.Open
        (File => Input_File, Mode => Ada.Text_IO.In_File, Name => File_Name);
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line
           ("Error: Input file '" & File_Name & "' not found.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         return;
   end;

   -- Read and process the file schematic by schematic until EOF.
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Current_Schematic : Schematic_Data;
         Line_Count        : Natural := 0;
         Is_Lock, Is_Key   : Boolean;
         Heights           : Pin_Heights;
      begin
         -- Read a block of lines for one schematic. A block is terminated
         -- by a blank line or the end of the file.
         while Line_Count < Schematic_Height
           and then not Ada.Text_IO.End_Of_File (Input_File)
         loop
            declare
               Line : String := Ada.Text_IO.Get_Line (Input_File);
            begin
               if Line'Length = 0 then
                  exit; -- Stop reading this block if we hit a blank line.
               end if;
               Line_Count := Line_Count + 1;
               Current_Schematic (Line_Count) := Line;
            end;
         end loop;

         -- If we read a full 7-line schematic, parse and store it.
         if Line_Count = Schematic_Height then
            Parse_Schematic (Current_Schematic, Is_Lock, Is_Key, Heights);
            if Is_Lock then
               Locks.Append (Heights);
            elsif Is_Key then
               Keys.Append (Heights);
            end if;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   -- Count the number of valid lock/key pairs.
   declare
      Fit_Count : Natural := 0;
   begin
      for Lock of Locks loop
         for Key of Keys loop
            if Fits (Lock, Key) then
               Fit_Count := Fit_Count + 1;
            end if;
         end loop;
      end loop;

      -- Print the final count to standard output.
      Ada.Integer_Text_IO.Put (Item => Fit_Count, Width => 0);
      Ada.Text_IO.New_Line;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("An unexpected error occurred: " & Ada.Exceptions.Exception_Name (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      if Ada.Text_IO.Is_Open (Input_File) then
         Ada.Text_IO.Close (Input_File);
      end if;
end Main;
