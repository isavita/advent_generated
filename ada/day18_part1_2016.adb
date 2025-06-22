
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;

procedure Main is

   Total_Rows : constant Natural := 40;

   -- Function to read the first row from a file
   function Read_First_Row (Filename : String) return Ada.Strings.Unbounded.Unbounded_String is
      File_Handle : Ada.Text_IO.File_Type;
      Line_Buffer : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Open (File_Handle, Ada.Text_IO.In_File, Filename);
      Line_Buffer := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Text_IO.Get_Line (File_Handle));
      Ada.Text_IO.Close (File_Handle);
      return Line_Buffer;
   exception
      when Ada.Text_IO.End_Error =>
         Ada.Text_IO.Put_Line ("Error: Input file is empty or could not read line.");
         raise Program_Error;
      when others =>
         Ada.Text_IO.Put_Line ("Error opening or reading file: " & Filename);
         raise Program_Error;
   end Read_First_Row;

   -- Helper function for character at index, handling out-of-bounds
   function Safe_If_Out_Of_Bounds (Index : Integer; Row : String) return Character is
   begin
      if Index < Row'First or else Index > Row'Last then
         return '.';
      else
         return Row (Index);
      end if;
   end Safe_If_Out_Of_Bounds;

   -- Function to determine if a position is a trap based on its neighbors
   function Is_Trap (Left_Idx, Center_Idx, Right_Idx : Integer; Row : String) return Boolean is
      L : Character := Safe_If_Out_Of_Bounds (Left_Idx, Row);
      C : Character := Safe_If_Out_Of_Bounds (Center_Idx, Row);
      R : Character := Safe_If_Out_Of_Bounds (Right_Idx, Row);
   begin
      return (L = '^' and C = '^' and R = '.') or else
             (C = '^' and R = '^' and L = '.') or else
             (L = '^' and C = '.' and R = '.') or else
             (R = '^' and C = '.' and L = '.');
   end Is_Trap;

   -- Function to count occurrences of a character in a string
   function Count_Char (Str : String; Char : Character) return Natural is
      Count : Natural := 0;
   begin
      for I in Str'Range loop
         if Str (I) = Char then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Count_Char;

   -- Function to count safe tiles over multiple rows
   function Count_Safe_Tiles (First_Row : String; Total_Rows : Natural) return Natural is
      Row_Length  : constant Natural := First_Row'Length;
      Current_Row : String (1 .. Row_Length);
      Next_Row    : String (1 .. Row_Length);
      Safe_Count  : Natural := 0;
   begin
      Current_Row := First_Row;
      Safe_Count  := Count_Char (Current_Row, '.');

      for I in 2 .. Total_Rows loop -- Loop from the 2nd row up to Total_Rows
         for J in 1 .. Row_Length loop -- Iterate through columns (1-based index)
            if Is_Trap (J - 1, J, J + 1, Current_Row) then
               Next_Row (J) := '^';
            else
               Next_Row (J) := '.';
               Safe_Count := Safe_Count + 1;
            end if;
         end loop;
         Current_Row := Next_Row;
      end loop;
      return Safe_Count;
   end Count_Safe_Tiles;

   -- Main program variables
   Unbounded_First_Row : Ada.Strings.Unbounded.Unbounded_String;
   Result              : Natural;

begin
   Unbounded_First_Row := Read_First_Row ("input.txt");

   -- Declare Fixed_First_Row within a block to size it dynamically based on input
   declare
      Row_Length_Val  : constant Natural := Ada.Strings.Unbounded.Length (Unbounded_First_Row);
      Fixed_First_Row : String (1 .. Row_Length_Val) := Ada.Strings.Unbounded.To_String (Unbounded_First_Row);
   begin
      Result := Count_Safe_Tiles (Fixed_First_Row, Total_Rows);
   end; -- End of inner block

   Ada.Integer_Text_IO.Put (Result);
   Ada.Text_IO.New_Line;

end Main;
