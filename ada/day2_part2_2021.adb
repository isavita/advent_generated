
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   Input_File          : Ada.Text_IO.File_Type;
   Horizontal_Position : Integer := 0;
   Depth               : Integer := 0;
   Aim                 : Integer := 0;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line        : constant String  := Ada.Text_IO.Get_Line (Input_File);
         Space_Index : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
         Direction   : constant String  := Line (Line'First .. Space_Index - 1);
         Units       : constant Integer := Integer'Value (Line (Space_Index + 1 .. Line'Last));
      begin
         if Direction = "forward" then
            Horizontal_Position := Horizontal_Position + Units;
            Depth               := Depth + Aim * Units;
         elsif Direction = "down" then
            Aim := Aim + Units;
         elsif Direction = "up" then
            Aim := Aim - Units;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Horizontal_Position * Depth, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
