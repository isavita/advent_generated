
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input    : Ada.Text_IO.File_Type;
   Checksum : Long_Integer := 0;
begin
   Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (Input) loop
      declare
         Line     : constant String := Ada.Text_IO.Get_Line (Input);
         Min_Val  : Integer         := Integer'Last;
         Max_Val  : Integer         := Integer'First;
         Value    : Integer;
         Index    : Positive        := Line'First;
         Last     : Natural;
         Has_Data : Boolean         := False;
      begin
         begin
            while Index <= Line'Last loop
               Ada.Integer_Text_IO.Get
                 (From => Line (Index .. Line'Last), Item => Value, Last => Last);
               Has_Data := True;
               Min_Val  := Integer'Min (Min_Val, Value);
               Max_Val  := Integer'Max (Max_Val, Value);
               Index    := Last + 1;
            end loop;
         exception
            when Ada.Text_IO.Data_Error =>
               null;
         end;

         if Has_Data then
            Checksum := Checksum + Long_Integer (Max_Val - Min_Val);
         end if;
      exception
         when Constraint_Error =>
            null;
      end;
   end loop;

   Ada.Text_IO.Close (Input);
   Ada.Text_IO.Put_Line (Checksum'Image (2 .. Checksum'Image'Length));
end Main;
