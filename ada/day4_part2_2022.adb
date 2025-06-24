
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   File  : Ada.Text_IO.File_Type;
   Count : Natural := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (File);
         Comma_Pos   : constant Natural := Ada.Strings.Fixed.Index (Line, ",");
         First_Part  : constant String := Line (Line'First .. Comma_Pos - 1);
         Second_Part : constant String := Line (Comma_Pos + 1 .. Line'Last);

         Hyphen_1_Pos : constant Natural := Ada.Strings.Fixed.Index (First_Part, "-");
         Hyphen_2_Pos : constant Natural := Ada.Strings.Fixed.Index (Second_Part, "-");

         L_Start : constant Integer :=
           Integer'Value (First_Part (First_Part'First .. Hyphen_1_Pos - 1));
         L_End : constant Integer :=
           Integer'Value (First_Part (Hyphen_1_Pos + 1 .. First_Part'Last));
         R_Start : constant Integer :=
           Integer'Value (Second_Part (Second_Part'First .. Hyphen_2_Pos - 1));
         R_End : constant Integer :=
           Integer'Value (Second_Part (Hyphen_2_Pos + 1 .. Second_Part'Last));
      begin
         if L_Start <= R_End and then L_End >= R_Start then
            Count := Count + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Item => Count);
   Ada.Text_IO.New_Line;
end Main;
