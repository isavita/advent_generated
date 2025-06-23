
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is
   Input_File : Ada.Text_IO.File_Type;
   Num_Twos   : Natural := 0;
   Num_Threes : Natural := 0;
   type Char_Counts is array (Character) of Natural;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line      : constant String := Ada.Text_IO.Get_Line (Input_File);
         Counts    : Char_Counts := (others => 0);
         Has_Two   : Boolean := False;
         Has_Three : Boolean := False;
      begin
         for C of Line loop
            Counts (C) := Counts (C) + 1;
         end loop;

         for C in Character'Range loop
            if Counts (C) = 2 then
               Has_Two := True;
            elsif Counts (C) = 3 then
               Has_Three := True;
            end if;
            exit when Has_Two and Has_Three;
         end loop;

         if Has_Two then
            Num_Twos := Num_Twos + 1;
         end if;
         if Has_Three then
            Num_Threes := Num_Threes + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);
   Ada.Integer_Text_IO.Put (Item => Num_Twos * Num_Threes);
   Ada.Text_IO.New_Line;
end Main;
