
with Ada.Text_IO;

procedure Main is
   File           : Ada.Text_IO.File_Type;
   Message_Length : Natural;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   Message_Length := Ada.Text_IO.Get_Line (File)'Length;

   if Message_Length = 0 then
      Ada.Text_IO.Close (File);
      return;
   end if;

   Ada.Text_IO.Reset (File);

   declare
      type Freq_Map is array (Character) of Natural;
      Freqs  : array (1 .. Message_Length) of Freq_Map := (others => (others => 0));
      Result : String (1 .. Message_Length);
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            for I in 1 .. Message_Length loop
               Freqs (I) (Line (I)) := Freqs (I) (Line (I)) + 1;
            end loop;
         end;
      end loop;
      Ada.Text_IO.Close (File);

      for I in 1 .. Message_Length loop
         declare
            Max_Count : Natural   := 0;
            Best_Char : Character := ' ';
         begin
            for C in Character'Range loop
               if Freqs (I) (C) > Max_Count then
                  Max_Count := Freqs (I) (C);
                  Best_Char := C;
               end if;
            end loop;
            Result (I) := Best_Char;
         end;
      end loop;

      Ada.Text_IO.Put_Line (Result);
   end;
exception
   when Ada.Text_IO.End_Error =>
      if Ada.Text_IO.Is_Open (File) then
         Ada.Text_IO.Close (File);
      end if;
end Main;
