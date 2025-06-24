
with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Main is
   package Integer_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Integer,
      Hash                => Ada.Containers.Hash_Type'Mod,
      Equivalent_Elements => "=");

   Input_File   : Ada.Text_IO.File_Type;
   Seen_Numbers : Integer_Sets.Set;
   Number       : Integer;
   Target       : Integer;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);
      begin
         if Line'Length > 0 then
            Number := Integer'Value (Line);
            Target := 2020 - Number;

            if Seen_Numbers.Contains (Target) then
               Ada.Text_IO.Put_Line (Integer'Image (Number * Target));
               Ada.Text_IO.Close (Input_File);
               return;
            end if;

            Seen_Numbers.Insert (Number);
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

exception
   when Ada.Text_IO.Name_Error | Ada.Text_IO.Data_Error | Constraint_Error =>
      null;
end Main;
