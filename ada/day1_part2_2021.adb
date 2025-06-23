
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Integer);

   File   : Ada.Text_IO.File_Type;
   Depths : Integer_Vectors.Vector;

   Increased_Measurements : Natural := 0;
   Increased_Sums         : Natural := 0;
begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : String := Ada.Text_IO.Get_Line (File);
      begin
         if Line'Length > 0 then
            Depths.Append (Integer'Value (Line));
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (File);

   for I in Depths.First_Index + 1 .. Depths.Last_Index loop
      if Depths.Element (I) > Depths.Element (I - 1) then
         Increased_Measurements := Increased_Measurements + 1;
      end if;
   end loop;

   for I in Depths.First_Index + 3 .. Depths.Last_Index loop
      if Depths.Element (I) > Depths.Element (I - 3) then
         Increased_Sums := Increased_Sums + 1;
      end if;
   end loop;

   Ada.Integer_Text_IO.Put (Item => Increased_Measurements, Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Put (Item => Increased_Sums, Width => 1);
   Ada.Text_IO.New_Line;

end Main;
