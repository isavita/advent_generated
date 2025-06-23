
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   Input_File      : Ada.Text_IO.File_Type;
   Valid_Passwords : Natural := 0;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (Input_File);
         Dash_Index  : constant Natural := Ada.Strings.Fixed.Index (Line, "-");
         Space_Index : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
         Colon_Index : constant Natural := Ada.Strings.Fixed.Index (Line, ":");

         Min_Count   : constant Positive := Integer'Value (Line (Line'First .. Dash_Index - 1));
         Max_Count   : constant Positive := Integer'Value (Line (Dash_Index + 1 .. Space_Index - 1));
         Letter      : constant Character := Line (Space_Index + 1);
         Password    : constant String := Line (Colon_Index + 2 .. Line'Last);

         Count       : Natural := 0;
      begin
         for C of Password loop
            if C = Letter then
               Count := Count + 1;
            end if;
         end loop;

         if Count in Min_Count .. Max_Count then
            Valid_Passwords := Valid_Passwords + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Valid_Passwords, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
