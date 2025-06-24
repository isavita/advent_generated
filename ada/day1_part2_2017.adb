
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File : Ada.Text_IO.File_Type;
   Total      : Natural := 0;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   declare
      Data        : constant String := Ada.Text_IO.Get_Line (Input_File);
      Length      : constant Natural := Data'Length;
      Half_Length : constant Natural := Length / 2;
   begin
      Ada.Text_IO.Close (Input_File);
      if Length > 0 then
         for I in Data'Range loop
            if Data (I) = Data (((I - 1 + Half_Length) mod Length) + 1) then
               Total := Total + Character'Pos (Data (I)) - Character'Pos ('0');
            end if;
         end loop;
      end if;
   end;
   Ada.Integer_Text_IO.Put (Total);
   Ada.Text_IO.New_Line;
exception
   when Ada.Text_IO.End_Error =>
      Ada.Integer_Text_IO.Put (0);
      Ada.Text_IO.New_Line;
end Main;
