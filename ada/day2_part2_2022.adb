
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   File        : Ada.Text_IO.File_Type;
   Total_Score : Natural := 0;
begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line         : constant String  := Ada.Text_IO.Get_Line (File);
         Opponent_Val : constant Integer := Character'Pos (Line (Line'First)) - Character'Pos ('A');
         Outcome_Val  : constant Integer := Character'Pos (Line (Line'First + 2)) - Character'Pos ('X');
         Shape_Score  : constant Integer := ((Opponent_Val + Outcome_Val - 1 + 3) mod 3) + 1;
         Outcome_Score: constant Integer := Outcome_Val * 3;
      begin
         Total_Score := Total_Score + Shape_Score + Outcome_Score;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Total_Score);
   Ada.Text_IO.New_Line;
end Main;
