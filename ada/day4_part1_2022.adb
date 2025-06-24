
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   procedure Parse_Range (R : in String; Start, Finish : out Integer) is
      Hyphen_Pos : constant Natural := Ada.Strings.Fixed.Index (R, "-");
   begin
      Start  := Integer'Value (R (R'First .. Hyphen_Pos - 1));
      Finish := Integer'Value (R (Hyphen_Pos + 1 .. R'Last));
   end Parse_Range;

   File   : Ada.Text_IO.File_Type;
   Count  : Natural := 0;
   Start1, End1, Start2, End2 : Integer;
begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line       : constant String := Ada.Text_IO.Get_Line (File);
         Comma_Pos  : constant Natural := Ada.Strings.Fixed.Index (Line, ",");
      begin
         if Comma_Pos > 0 then
            Parse_Range (Line (Line'First .. Comma_Pos - 1), Start1, End1);
            Parse_Range (Line (Comma_Pos + 1 .. Line'Last), Start2, End2);

            if (Start1 <= Start2 and then End1 >= End2) or else
               (Start2 <= Start1 and then End2 >= End1)
            then
               Count := Count + 1;
            end if;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Item => Count, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
