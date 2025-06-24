
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is

   function Is_Valid (Password : Integer) return Boolean is
      Num         : Integer := Password;
      Has_Double  : Boolean := False;
      Run_Length  : Natural := 1;
      Prev_Digit  : Integer := 10;
   begin
      while Num > 0 loop
         declare
            Digit : constant Integer := Num mod 10;
         begin
            if Digit > Prev_Digit then
               return False;
            end if;

            if Digit = Prev_Digit then
               Run_Length := Run_Length + 1;
            else
               if Run_Length = 2 then
                  Has_Double := True;
               end if;
               Run_Length := 1;
            end if;

            Prev_Digit := Digit;
            Num        := Num / 10;
         end;
      end loop;
      return Has_Double or else (Run_Length = 2);
   end Is_Valid;

   File  : Ada.Text_IO.File_Type;
   Count : Natural := 0;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   declare
      Line       : constant String  := Ada.Text_IO.Get_Line (File);
      Hyphen_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, "-");
      Start_Val  : constant Integer := Integer'Value (Line (Line'First .. Hyphen_Pos - 1));
      End_Val    : constant Integer := Integer'Value (Line (Hyphen_Pos + 1 .. Line'Last));
   begin
      Ada.Text_IO.Close (File);
      for I in Start_Val .. End_Val loop
         if Is_Valid (I) then
            Count := Count + 1;
         end if;
      end loop;
   end;

   Ada.Integer_Text_IO.Put (Item => Count, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
