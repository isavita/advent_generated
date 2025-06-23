
with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;

procedure Main is
   type Secret_Type is mod 2**24;
   type Transform_Basis is array (0 .. 23) of Secret_Type;

   function Next_Secret (S : Secret_Type) return Secret_Type is
      pragma Inline (Next_Secret);
      S1 : Secret_Type := S xor (S * 64);
      S2 : Secret_Type := S1 xor (S1 / 32);
   begin
      return S2 xor (S2 * 2048);
   end Next_Secret;

   Basis_Map  : Transform_Basis;
   Input_File : Ada.Text_IO.File_Type;
   Total      : Long_Long_Integer := 0;
begin
   for I in Basis_Map'Range loop
      declare
         S : Secret_Type := 2**I;
      begin
         for J in 1 .. 2000 loop
            S := Next_Secret (S);
         end loop;
         Basis_Map (I) := S;
      end;
   end loop;

   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line   : String := Ada.Text_IO.Get_Line (Input_File);
         Result : Secret_Type := 0;
         Temp_B : Secret_Type;
      begin
         if Line'Length > 0 then
            Temp_B := Secret_Type'Value (Line);
            for I in Basis_Map'Range loop
               if (Temp_B and 1) = 1 then
                  Result := Result xor Basis_Map (I);
               end if;
               Temp_B := Temp_B / 2;
            end loop;
            Total := Total + Long_Long_Integer (Result);
         end if;
      end;
   end loop;
   Ada.Text_IO.Close (Input_File);

   Ada.Long_Long_Integer_Text_IO.Put (Item => Total, Width => 0);
   Ada.Text_IO.New_Line;
end Main;
