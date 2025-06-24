
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Integer);

   File    : Ada.Text_IO.File_Type;
   Offsets : Integer_Vectors.Vector;
   Index   : Integer;
   Steps   : Natural := 0;
   Jump    : Integer;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         if Line'Length > 0 then
            Offsets.Append (Integer'Value (Line));
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (File);

   if Offsets.Is_Empty then
      Ada.Integer_Text_IO.Put (0);
      Ada.Text_IO.New_Line;
      return;
   end if;

   Index := Offsets.First_Index;

   while Index >= Offsets.First_Index and then Index <= Offsets.Last_Index loop
      Jump := Offsets.Element (Index);

      if Jump >= 3 then
         Offsets.Replace_Element (Index, Jump - 1);
      else
         Offsets.Replace_Element (Index, Jump + 1);
      end if;

      Index := Index + Jump;
      Steps := Steps + 1;
   end loop;

   Ada.Integer_Text_IO.Put (Item => Steps, Width => 1);
   Ada.Text_IO.New_Line;

end Main;
