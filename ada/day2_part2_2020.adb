
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Solve is
   File        : Ada.Text_IO.File_Type;
   Valid_Count : Natural := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (File);
         Dash_Index  : constant Natural := Ada.Strings.Fixed.Index (Line, "-");
         Space_Index : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
         Colon_Index : constant Natural := Ada.Strings.Fixed.Index (Line, ": ");

         Pos1        : constant Positive := Integer'Value (Line (Line'First .. Dash_Index - 1));
         Pos2        : constant Positive := Integer'Value (Line (Dash_Index + 1 .. Space_Index - 1));
         Letter      : constant Character := Line (Space_Index + 1);
         Password    : constant String := Line (Colon_Index + 2 .. Line'Last);
      begin
         if (Password (Password'First + Pos1 - 1) = Letter) xor
            (Password (Password'First + Pos2 - 1) = Letter)
         then
            Valid_Count := Valid_Count + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Valid_Count, 0);
   Ada.Text_IO.New_Line;
end Solve;
