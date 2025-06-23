
with Ada.Text_IO, Ada.Integer_Text_IO;

procedure Main is
   Input_File : Ada.Text_IO.File_Type;
   Nice_Count : Natural := 0;

   function Has_Non_Overlapping_Pair (S : String) return Boolean is
   begin
      for I in S'First .. S'Last - 3 loop
         for J in I + 2 .. S'Last - 1 loop
            if S (I .. I + 1) = S (J .. J + 1) then
               return True;
            end if;
         end loop;
      end loop;
      return False;
   end Has_Non_Overlapping_Pair;

   function Has_Repeat_With_Gap (S : String) return Boolean is
   begin
      for I in S'First .. S'Last - 2 loop
         if S (I) = S (I + 2) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Repeat_With_Gap;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);
      begin
         if Has_Repeat_With_Gap (Line) and then Has_Non_Overlapping_Pair (Line) then
            Nice_Count := Nice_Count + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Nice_Count, Width => 0);
   Ada.Text_IO.New_Line;
end Main;
