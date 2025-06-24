
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File  : Ada.Text_IO.File_Type;
   Top_Three   : array (1 .. 3) of Natural := (0, 0, 0);
   Current_Sum : Natural := 0;

   procedure Update_Top_Three is
   begin
      if Current_Sum > Top_Three (1) then
         Top_Three (3) := Top_Three (2);
         Top_Three (2) := Top_Three (1);
         Top_Three (1) := Current_Sum;
      elsif Current_Sum > Top_Three (2) then
         Top_Three (3) := Top_Three (2);
         Top_Three (2) := Current_Sum;
      elsif Current_Sum > Top_Three (3) then
         Top_Three (3) := Current_Sum;
      end if;
   end Update_Top_Three;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);
      begin
         if Line'Length = 0 then
            Update_Top_Three;
            Current_Sum := 0;
         else
            Current_Sum := Current_Sum + Integer'Value (Line);
         end if;
      end;
   end loop;

   Update_Top_Three;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Top_Three (1) + Top_Three (2) + Top_Three (3));
   Ada.Text_IO.New_Line;

end Main;
