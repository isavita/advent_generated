
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   subtype Seat_ID is Natural range 0 .. 1023;

   function Get_Seat_ID (Pass : String) return Seat_ID is
      ID : Seat_ID := 0;
   begin
      for C of Pass loop
         ID := ID * 2;
         if C = 'B' or else C = 'R' then
            ID := ID + 1;
         end if;
      end loop;
      return ID;
   end Get_Seat_ID;

   File         : Ada.Text_IO.File_Type;
   Max_ID       : Seat_ID := Seat_ID'First;
   Min_ID       : Seat_ID := Seat_ID'Last;
   Sum_Of_IDs   : Long_Integer := 0;
   Line         : String (1 .. 10);
   Last         : Natural;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, Line, Last);
      if Last = 10 then
         declare
            Current_ID : constant Seat_ID := Get_Seat_ID (Line);
         begin
            Max_ID     := Seat_ID'Max (Max_ID, Current_ID);
            Min_ID     := Seat_ID'Min (Min_ID, Current_ID);
            Sum_Of_IDs := Sum_Of_IDs + Long_Integer(Current_ID);
         end;
      end if;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Max_ID, Width => 1);
   Ada.Text_IO.New_Line;

   declare
      Expected_Sum : constant Long_Integer :=
        (Long_Integer(Max_ID) - Long_Integer(Min_ID) + 1) *
        (Long_Integer(Min_ID) + Long_Integer(Max_ID)) / 2;
      Missing_ID   : constant Seat_ID := Seat_ID (Expected_Sum - Sum_Of_IDs);
   begin
      Ada.Integer_Text_IO.Put (Item => Missing_ID, Width => 1);
      Ada.Text_IO.New_Line;
   end;

end Main;
