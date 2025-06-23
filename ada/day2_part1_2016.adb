
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   Input      : Ada.Text_IO.File_Type;
   X, Y       : Integer := 1;
   Keypad     : constant array (0 .. 2, 0 .. 2) of Character :=
     (('1', '4', '7'), ('2', '5', '8'), ('3', '6', '9'));
   Code       : Ada.Strings.Unbounded.Unbounded_String;
begin
   Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (Input) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input);
      begin
         for Move of Line loop
            case Move is
               when 'U' => Y := Integer'Max (0, Y - 1);
               when 'D' => Y := Integer'Min (2, Y + 1);
               when 'L' => X := Integer'Max (0, X - 1);
               when 'R' => X := Integer'Min (2, X + 1);
               when others => null;
            end case;
         end loop;
         Ada.Strings.Unbounded.Append (Code, Keypad (X, Y));
      end;
   end loop;

   Ada.Text_IO.Close (Input);
   Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Code));
end Main;
