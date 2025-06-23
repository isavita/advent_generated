
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   type Grid_Type is array (Positive range <>, Positive range <>) of Character;

   function Get_Next_State (Grid : Grid_Type; Row, Col : Positive) return Character is
      Neighbors : Natural := 0;
   begin
      for R in Row - 1 .. Row + 1 loop
         for C in Col - 1 .. Col + 1 loop
            if R in Grid'Range (1) and then C in Grid'Range (2) and then
              (R /= Row or else C /= Col)
            then
               if Grid (R, C) = '#' then
                  Neighbors := Neighbors + 1;
               end if;
            end if;
         end loop;
      end loop;

      if Grid (Row, Col) = '#' then
         if Neighbors = 2 or else Neighbors = 3 then
            return '#';
         else
            return '.';
         end if;
      else
         if Neighbors = 3 then
            return '#';
         else
            return '.';
         end if;
      end if;
   end Get_Next_State;

   File           : Ada.Text_IO.File_Type;
   File_Name      : constant String := "input.txt";
   Num_Rows, Num_Cols : Natural := 0;

begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => File_Name);
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
      begin
         if Num_Rows = 0 then
            Num_Cols := Line'Length;
         end if;
         Num_Rows := Num_Rows + 1;
      end;
   end loop;
   Ada.Text_IO.Reset (File, Mode => Ada.Text_IO.In_File);

   declare
      Grid, Next_Grid : Grid_Type (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for R in Grid'Range (1) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File);
         begin
            for C in Grid'Range (2) loop
               Grid (R, C) := Line (Line'First + C - 1);
            end loop;
         end;
      end loop;
      Ada.Text_IO.Close (File);

      for I in 1 .. 100 loop
         for R in Grid'Range (1) loop
            for C in Grid'Range (2) loop
               Next_Grid (R, C) := Get_Next_State (Grid, R, C);
            end loop;
         end loop;
         Grid := Next_Grid;
      end loop;

      declare
         On_Count : Natural := 0;
      begin
         for R in Grid'Range (1) loop
            for C in Grid'Range (2) loop
               if Grid (R, C) = '#' then
                  On_Count := On_Count + 1;
               end if;
            end loop;
         end loop;
         Ada.Integer_Text_IO.Put (Item => On_Count, Width => 1);
         Ada.Text_IO.New_Line;
      end;
   end;
end Main;
