
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Integer);
   use Integer_Vectors;

   File      : Ada.Text_IO.File_Type;
   Checksum1 : Natural := 0;
   Checksum2 : Natural := 0;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (File);
         Row         : Vector;
         Start_Index : Positive := Line'First;
         End_Index   : Natural;
         Min_Val     : Integer;
         Max_Val     : Integer;
      begin
         while Start_Index <= Line'Last loop
            while Start_Index <= Line'Last
              and then
                (Line (Start_Index) = ' ' or else Line (Start_Index) = Character'Val (9))
            loop
               Start_Index := Start_Index + 1;
            end loop;
            if Start_Index > Line'Last then exit; end if;

            End_Index := Start_Index;
            while End_Index <= Line'Last
              and then
                Line (End_Index) /= ' ' and then Line (End_Index) /= Character'Val (9)
            loop
               End_Index := End_Index + 1;
            end loop;
            Row.Append (Integer'Value (Line (Start_Index .. End_Index - 1)));
            Start_Index := End_Index;
         end loop;

         if not Row.Is_Empty then
            Min_Val := Row.First_Element;
            Max_Val := Row.First_Element;
            for E of Row loop
               if E < Min_Val then Min_Val := E; end if;
               if E > Max_Val then Max_Val := E; end if;
            end loop;
            Checksum1 := Checksum1 + (Max_Val - Min_Val);

            for I in Row.First_Index .. Row.Last_Index loop
               for J in I + 1 .. Row.Last_Index loop
                  declare
                     Val1 : constant Integer := Row.Element (I);
                     Val2 : constant Integer := Row.Element (J);
                  begin
                     if Val1 mod Val2 = 0 then
                        Checksum2 := Checksum2 + (Val1 / Val2);
                     elsif Val2 mod Val1 = 0 then
                        Checksum2 := Checksum2 + (Val2 / Val1);
                     end if;
                  end;
               end loop;
            end loop;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Item => Checksum1, Width => 1);
   Ada.Text_IO.New_Line;
   Ada.Integer_Text_IO.Put (Item => Checksum2, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
