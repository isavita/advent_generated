
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Interfaces;

procedure Main is
   use Interfaces;

   function Item_Bit_Position (Item : Character) return Natural is
   begin
      if Item in 'a' .. 'z' then
         return Character'Pos (Item) - Character'Pos ('a');
      else
         return Character'Pos (Item) - Character'Pos ('A') + 26;
      end if;
   end Item_Bit_Position;

   File        : Ada.Text_IO.File_Type;
   Sum         : Natural := 0;
   Group_Masks : array (0 .. 2) of Unsigned_64 := (others => 0);
   Line_Index  : Natural := 0;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (File);
         Mask : Unsigned_64 := 0;
      begin
         for C of Line loop
            Mask := Mask or Shift_Left (1, Item_Bit_Position (C));
         end loop;
         Group_Masks (Line_Index mod 3) := Mask;
      end;

      if Line_Index mod 3 = 2 then
         declare
            Common_Mask : constant Unsigned_64 :=
              Group_Masks (0) and Group_Masks (1) and Group_Masks (2);
         begin
            for I in 0 .. 51 loop
               if (Common_Mask and Shift_Left (1, I)) /= 0 then
                  Sum := Sum + I + 1;
                  exit;
               end if;
            end loop;
         end;
      end if;

      Line_Index := Line_Index + 1;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Sum);
   Ada.Text_IO.New_Line;
end Main;
