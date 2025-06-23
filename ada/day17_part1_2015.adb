
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Integer);

   Target     : constant := 150;
   Containers : Integer_Vectors.Vector;
   Input_File : Ada.Text_IO.File_Type;

   subtype DP_Range is Integer range 0 .. Target;
   type DP_Array is array (DP_Range) of Natural;
   DP : DP_Array := (0 => 1, others => 0);

begin
   Ada.Text_IO.Open
     (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);
      begin
         if Line'Length > 0 then
            Containers.Append (Integer'Value (Line));
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (Input_File);

   for C of Containers loop
      for I in reverse C .. Target loop
         DP (I) := DP (I) + DP (I - C);
      end loop;
   end loop;

   Ada.Integer_Text_IO.Put (Item => DP (Target), Width => 1);
   Ada.Text_IO.New_Line;
end Main;
