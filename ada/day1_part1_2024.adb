
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Integer);

   package Integer_Sorting is new Integer_Vectors.Generic_Sorting ("<");

   Input_File           : Ada.Text_IO.File_Type;
   Locations1, Locations2 : Integer_Vectors.Vector;
   Total_Distance       : Long_Integer := 0;

begin
   Ada.Text_IO.Open (Input_File, Ada.Text_IO.In_File, "input.txt");

   loop
      declare
         Val1, Val2 : Integer;
      begin
         Ada.Integer_Text_IO.Get (File => Input_File, Item => Val1);
         Ada.Integer_Text_IO.Get (File => Input_File, Item => Val2);
         Locations1.Append (Val1);
         Locations2.Append (Val2);
      exception
         when Ada.Text_IO.End_Error =>
            exit;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Integer_Sorting.Sort (Locations1);
   Integer_Sorting.Sort (Locations2);

   for I in Locations1.First_Index .. Locations1.Last_Index loop
      Total_Distance :=
        Total_Distance +
        Long_Integer (abs (Locations1.Element (I) - Locations2.Element (I)));
   end loop;

   Ada.Long_Integer_Text_IO.Put (Total_Distance);
   Ada.Text_IO.New_Line;

end Main;
