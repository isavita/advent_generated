
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Solve is
   File        : Ada.Text_IO.File_Type;
   Current_Sum : Natural := 0;
   Max_Sum     : Natural := 0;
begin
   Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         if Line'Length = 0 then
            if Current_Sum > Max_Sum then
               Max_Sum := Current_Sum;
            end if;
            Current_Sum := 0;
         else
            Current_Sum := Current_Sum + Integer'Value (Line);
         end if;
      end;
   end loop;

   if Current_Sum > Max_Sum then
      Max_Sum := Current_Sum;
   end if;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Max_Sum);
   Ada.Text_IO.New_Line;
end Solve;
