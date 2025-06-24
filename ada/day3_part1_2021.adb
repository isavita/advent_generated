
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Solve is
   File         : Ada.Text_IO.File_Type;
   Line_Count   : Natural := 0;
   Bit_Length   : Positive;
   Gamma_Rate   : Natural := 0;
   Epsilon_Rate : Natural := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   declare
      First_Line : constant String := Ada.Text_IO.Get_Line (File);
   begin
      Bit_Length := First_Line'Length;
      Ada.Text_IO.Reset (File, Ada.Text_IO.In_File);
   end;

   declare
      Bit_Counts : array (1 .. Bit_Length) of Natural := (others => 0);
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            Line_Count := Line_Count + 1;
            for I in Line'Range loop
               if Line (I) = '1' then
                  Bit_Counts (I) := Bit_Counts (I) + 1;
               end if;
            end loop;
         end;
      end loop;

      Ada.Text_IO.Close (File);

      for I in Bit_Counts'Range loop
         Gamma_Rate   := Gamma_Rate * 2;
         Epsilon_Rate := Epsilon_Rate * 2;
         if Bit_Counts (I) * 2 > Line_Count then
            Gamma_Rate := Gamma_Rate + 1;
         else
            Epsilon_Rate := Epsilon_Rate + 1;
         end if;
      end loop;

      Ada.Integer_Text_IO.Put (Item => Gamma_Rate * Epsilon_Rate, Width => 0);
      Ada.Text_IO.New_Line;
   end;

end Solve;
