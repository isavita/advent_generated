
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Limit        : constant := 1_000_000;
   Puzzle_Input : Positive;
   File         : Ada.Text_IO.File_Type;

   type Presents_Array is array (Positive range 1 .. Limit) of Natural;
   Presents : Presents_Array := (others => 0);
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   Ada.Integer_Text_IO.Get (File, Puzzle_Input);
   Ada.Text_IO.Close (File);

   for Elf in 1 .. Limit loop
      for I in 1 .. Limit / Elf loop
         Presents (I * Elf) := Presents (I * Elf) + Elf * 10;
      end loop;
   end loop;

   for House_Num in Presents'Range loop
      if Presents (House_Num) >= Puzzle_Input then
         Ada.Integer_Text_IO.Put (House_Num, 1);
         Ada.Text_IO.New_Line;
         return;
      end if;
   end loop;
end Main;
