
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input      : Ada.Text_IO.File_Type;
   Total_Fuel : Integer := 0;
begin
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input) loop
      declare
         Mass : constant Integer := Integer'Value (Ada.Text_IO.Get_Line (Input));
      begin
         Total_Fuel := Total_Fuel + (Mass / 3 - 2);
      end;
   end loop;

   Ada.Text_IO.Close (Input);

   Ada.Integer_Text_IO.Put (Item => Total_Fuel);
   Ada.Text_IO.New_Line;
end Main;
