
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

procedure Main is
   File      : Ada.Text_IO.File_Type;
   Total_Sum : Natural := 0;

   Max_Red   : constant := 12;
   Max_Green : constant := 13;
   Max_Blue  : constant := 14;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line        : constant String := Ada.Text_IO.Get_Line (File);
         Index       : Positive       := Line'First;
         Game_ID     : Natural;
         Is_Possible : Boolean        := True;
      begin
         while Line (Index) /= ':' loop
            Index := Index + 1;
         end loop;
         Game_ID := Natural'Value (Line (Line'First + 5 .. Index - 1));
         Index   := Index + 1;

         while Index <= Line'Last and then Is_Possible loop
            while Index <= Line'Last and then
              not Ada.Characters.Handling.Is_Digit (Line (Index))
            loop
               Index := Index + 1;
            end loop;

            if Index > Line'Last then
               exit;
            end if;

            declare
               Start_Of_Num : constant Positive := Index;
               Current_Num  : Natural;
            begin
               while Index <= Line'Last and then
                 Ada.Characters.Handling.Is_Digit (Line (Index))
               loop
                  Index := Index + 1;
               end loop;
               Current_Num := Natural'Value (Line (Start_Of_Num .. Index - 1));
               Index       := Index + 1;

               case Line (Index) is
                  when 'r' =>
                     if Current_Num > Max_Red then Is_Possible := False; end if;
                  when 'g' =>
                     if Current_Num > Max_Green then Is_Possible := False; end if;
                  when 'b' =>
                     if Current_Num > Max_Blue then Is_Possible := False; end if;
                  when others =>
                     null;
               end case;
            end;
         end loop;

         if Is_Possible then
            Total_Sum := Total_Sum + Game_ID;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);
   Ada.Integer_Text_IO.Put (Total_Sum);
   Ada.Text_IO.New_Line;

end Main;
