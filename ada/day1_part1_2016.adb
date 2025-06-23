
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Solve is
   File  : Ada.Text_IO.File_Type;
   Line  : String (1 .. 4096);
   Last  : Natural;
   X, Y  : Integer  := 0;
   Index : Positive := 1;

   type Direction is mod 4;
   Current_Dir : Direction := 0;

   type Vector is record
      DX, DY : Integer;
   end record;

   Dirs : constant array (Direction) of Vector :=
     (0 => (DX => 0, DY => 1),
      1 => (DX => 1, DY => 0),
      2 => (DX => 0, DY => -1),
      3 => (DX => -1, DY => 0));

begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   Ada.Text_IO.Get_Line (File, Line, Last);
   Ada.Text_IO.Close (File);

   while Index <= Last loop
      while Index <= Last and (Line (Index) = ',' or Line (Index) = ' ') loop
         Index := Index + 1;
      end loop;

      if Index > Last then
         exit;
      end if;

      if Line (Index) = 'R' then
         Current_Dir := Current_Dir + 1;
      else
         Current_Dir := Current_Dir - 1;
      end if;
      Index := Index + 1;

      declare
         Start_Num : constant Positive := Index;
         Dist      : Integer;
      begin
         while Index <= Last and then Line (Index) in '0' .. '9' loop
            Index := Index + 1;
         end loop;
         Dist := Integer'Value (Line (Start_Num .. Index - 1));
         X := X + Dirs (Current_Dir).DX * Dist;
         Y := Y + Dirs (Current_Dir).DY * Dist;
      end;
   end loop;

   Ada.Integer_Text_IO.Put (Item => abs X + abs Y);
   Ada.Text_IO.New_Line;
end Solve;
