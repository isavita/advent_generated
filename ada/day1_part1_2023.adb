
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File : Ada.Text_IO.File_Type;
   Sum        : Natural := 0;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (Input_File);
         First_Digit : Integer := -1;
         Last_Digit  : Integer := -1;
      begin
         for C of Line loop
            if C in '0' .. '9' then
               if First_Digit = -1 then
                  First_Digit := Character'Pos (C) - Character'Pos ('0');
               end if;
               Last_Digit := Character'Pos (C) - Character'Pos ('0');
            end if;
         end loop;

         if First_Digit /= -1 then
            Sum := Sum + (First_Digit * 10 + Last_Digit);
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);
   Ada.Integer_Text_IO.Put (Item => Sum);
   Ada.Text_IO.New_Line;
end Main;
