
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   File  : Ada.Text_IO.File_Type;
   Total : Natural := 0;

   function Priority (Item : Character) return Natural is
   begin
      if Item in 'a' .. 'z' then
         return Character'Pos (Item) - Character'Pos ('a') + 1;
      else
         return Character'Pos (Item) - Character'Pos ('A') + 27;
      end if;
   end Priority;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line      : constant String := Ada.Text_IO.Get_Line (File);
         Mid       : constant Positive := Line'Length / 2;
         First_Set : array (Character) of Boolean := (others => False);
         Found     : array (Character) of Boolean := (others => False);
      begin
         for C of Line (Line'First .. Line'First + Mid - 1) loop
            First_Set (C) := True;
         end loop;

         for C of Line (Line'First + Mid .. Line'Last) loop
            if First_Set (C) and not Found (C) then
               Total := Total + Priority (C);
               Found (C) := True;
            end if;
         end loop;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Item => Total, Width => 0);
   Ada.Text_IO.New_Line;
end Main;
