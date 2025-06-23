
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input      : Ada.Text_IO.File_Type;
   Total_Fuel : Integer := 0;

   function Calculate_Fuel (Mass : Integer) return Integer is
      Module_Total : Integer := 0;
      Current_Fuel : Integer := Mass / 3 - 2;
   begin
      while Current_Fuel > 0 loop
         Module_Total := Module_Total + Current_Fuel;
         Current_Fuel := Current_Fuel / 3 - 2;
      end loop;
      return Module_Total;
   end Calculate_Fuel;

begin
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input) loop
      declare
         Line : constant String  := Ada.Text_IO.Get_Line (Input);
         Mass : constant Integer := Integer'Value (Line);
      begin
         Total_Fuel := Total_Fuel + Calculate_Fuel (Mass);
      end;
   end loop;

   Ada.Text_IO.Close (Input);

   Ada.Integer_Text_IO.Put (Item => Total_Fuel);
   Ada.Text_IO.New_Line;
end Main;
