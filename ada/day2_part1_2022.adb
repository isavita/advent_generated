
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   File           : Ada.Text_IO.File_Type;
   Total_Score    : Natural := 0;
   Outcome_Scores : constant array (0 .. 2) of Natural := (3, 6, 0);
begin
   Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line         : constant String  := Ada.Text_IO.Get_Line (File);
         Opponent_Val : constant Natural := Character'Pos (Line (Line'First)) - Character'Pos ('A');
         Your_Val     : constant Natural := Character'Pos (Line (Line'First + 2)) - Character'Pos ('X');
         Outcome      : constant Natural := (Your_Val - Opponent_Val + 3) mod 3;
      begin
         Total_Score := Total_Score + (Your_Val + 1) + Outcome_Scores (Outcome);
      end;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Total_Score, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
