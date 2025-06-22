
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;

procedure Main is
   Input_File    : Ada.Text_IO.File_Type;
   Ch            : Character;
   Score         : Integer := 0;
   Depth         : Integer := 0;
   In_Garbage    : Boolean := False;
   Cancel_Next   : Boolean := False;
   Garbage_Count : Integer := 0;
begin
   begin
      Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("File 'input.txt' not found.");
         return;
      when others =>
         Ada.Text_IO.Put_Line ("An error occurred opening 'input.txt'.");
         return;
   end;

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      Ada.Text_IO.Get (File => Input_File, Item => Ch);

      if Cancel_Next then
         Cancel_Next := False;
      else
         if In_Garbage then
            if Ch = '!' then
               Cancel_Next := True;
            elsif Ch = '>' then
               In_Garbage := False;
            else
               Garbage_Count := Garbage_Count + 1;
            end if;
         else
            case Ch is
               when '{' =>
                  Depth := Depth + 1;
               when '}' =>
                  Score := Score + Depth;
                  Depth := Depth - 1;
               when '<' =>
                  In_Garbage := True;
               when others =>
                  null;
            end case;
         end if;
      end if;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Garbage_Count);
   Ada.Text_IO.New_Line;
end Main;
