
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   File       : Ada.Text_IO.File_Type;
   Tree_Count : Natural := 0;
   X_Pos      : Natural := 0;
   Line_Width : Positive;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   declare
      First_Line : constant String := Ada.Text_IO.Get_Line (File);
   begin
      Line_Width := First_Line'Length;
   end;
   Ada.Text_IO.Reset (File);

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line  : constant String   := Ada.Text_IO.Get_Line (File);
         Index : constant Positive := (X_Pos mod Line_Width) + 1;
      begin
         if Line (Index) = '#' then
            Tree_Count := Tree_Count + 1;
         end if;
      end;
      X_Pos := X_Pos + 3;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Tree_Count, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
