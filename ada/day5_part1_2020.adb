
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File  : Ada.Text_IO.File_Type;
   Max_Seat_ID : Natural := 0;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line            : constant String := Ada.Text_IO.Get_Line (Input_File);
         Current_Seat_ID : Natural       := 0;
      begin
         for C of Line loop
            Current_Seat_ID := Current_Seat_ID * 2;
            if C = 'B' or else C = 'R' then
               Current_Seat_ID := Current_Seat_ID + 1;
            end if;
         end loop;
         Max_Seat_ID := Natural'Max (Max_Seat_ID, Current_Seat_ID);
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Max_Seat_ID, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
