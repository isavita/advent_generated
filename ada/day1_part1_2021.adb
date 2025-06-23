
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File : Ada.Text_IO.File_Type;
   Count      : Natural := 0;
   Previous   : Integer;
   Current    : Integer;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   if not Ada.Text_IO.End_Of_File (Input_File) then
      Ada.Integer_Text_IO.Get (File => Input_File, Item => Previous);

      while not Ada.Text_IO.End_Of_File (Input_File) loop
         Ada.Integer_Text_IO.Get (File => Input_File, Item => Current);
         if Current > Previous then
            Count := Count + 1;
         end if;
         Previous := Current;
      end loop;
   end if;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Count, Width => 1);
   Ada.Text_IO.New_Line;

exception
   when Ada.Text_IO.Name_Error =>
      null;
   when Ada.Text_IO.Data_Error =>
      null;
end Main;
