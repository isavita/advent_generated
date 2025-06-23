
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File         : Ada.Text_IO.File_Type;
   Possible_Triangles : Natural := 0;
   A, B, C            : Integer;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      Ada.Integer_Text_IO.Get (File => Input_File, Item => A);
      Ada.Integer_Text_IO.Get (File => Input_File, Item => B);
      Ada.Integer_Text_IO.Get (File => Input_File, Item => C);

      if A + B > C and A + C > B and B + C > A then
         Possible_Triangles := Possible_Triangles + 1;
      end if;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Possible_Triangles);
   Ada.Text_IO.New_Line;
end Main;
