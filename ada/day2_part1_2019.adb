
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;

procedure Main is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
   use Int_Vectors;

   File : Ada.Text_IO.File_Type;
   Data : Vector;
   Pos  : Natural := 0;

begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   declare
      Line        : constant String := Ada.Text_IO.Get_Line (File);
      Start_Index : Positive        := Line'First;
   begin
      for I in Line'Range loop
         if Line (I) = ',' then
            Data.Append (Integer'Value (Line (Start_Index .. I - 1)));
            Start_Index := I + 1;
         end if;
      end loop;
      Data.Append (Integer'Value (Line (Start_Index .. Line'Last)));
   end;
   Ada.Text_IO.Close (File);

   Data (1) := 12;
   Data (2) := 2;

   while Data (Pos) /= 99 loop
      case Data (Pos) is
         when 1 =>
            Data (Data (Pos + 3)) := Data (Data (Pos + 1)) + Data (Data (Pos + 2));
         when 2 =>
            Data (Data (Pos + 3)) := Data (Data (Pos + 1)) * Data (Data (Pos + 2));
         when others =>
            null;
      end case;
      Pos := Pos + 4;
   end loop;

   Ada.Integer_Text_IO.Put (Item => Data (0), Width => 1);
   Ada.Text_IO.New_Line;

end Main;
