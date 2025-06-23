
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   Target_Liters : constant Natural := 150;

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Natural);
   use Integer_Vectors;

   Containers : Vector;
   File       : Ada.Text_IO.File_Type;

   Total_Combinations         : Natural := 0;
   Min_Containers             : Natural := Natural'Last;
   Min_Container_Combinations : Natural := 0;

   procedure Find_Combinations (Index : Positive; Sum : Natural; Count : Natural) is
   begin
      if Sum = Target_Liters then
         Total_Combinations := Total_Combinations + 1;
         if Count < Min_Containers then
            Min_Containers             := Count;
            Min_Container_Combinations := 1;
         elsif Count = Min_Containers then
            Min_Container_Combinations := Min_Container_Combinations + 1;
         end if;
         return;
      end if;

      if Index > Containers.Last_Index or Sum > Target_Liters then
         return;
      end if;

      for I in Index .. Containers.Last_Index loop
         Find_Combinations (I + 1, Sum + Containers.Element (I), Count + 1);
      end loop;
   end Find_Combinations;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         if Line'Length > 0 then
            Containers.Append (Natural'Value (Line));
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (File);

   if not Containers.Is_Empty then
      Find_Combinations (Containers.First_Index, 0, 0);
   end if;

   Ada.Integer_Text_IO.Put (Item => Total_Combinations, Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Put (Item => Min_Container_Combinations, Width => 1);
   Ada.Text_IO.New_Line;

end Main;
