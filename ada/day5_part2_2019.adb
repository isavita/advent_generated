
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);
   use Int_Vectors;

   File         : Ada.Text_IO.File_Type;
   Instructions : Vector;
   Num_Str      : String (1 .. 10);
   Last         : Natural := 0;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   declare
      Line : String := Ada.Text_IO.Get_Line (File);
   begin
      for I in Line'Range loop
         if Line (I) = ',' then
            if Last > 0 then
               Instructions.Append (Integer'Value (Num_Str (1 .. Last)));
               Last := 0;
            end if;
         else
            Last       := Last + 1;
            Num_Str (Last) := Line (I);
         end if;
      end loop;
      if Last > 0 then
         Instructions.Append (Integer'Value (Num_Str (1 .. Last)));
      end if;
   end;
   Ada.Text_IO.Close (File);

   declare
      procedure Run_Program (Program : in out Vector; Input_Val : in Integer) is
         Index : Natural := 0;

         function Get_Value (Param_Index : Natural; Mode : Integer) return Integer is
         begin
            if Mode = 0 then
               return Program.Element (Program.Element (Param_Index));
            else
               return Program.Element (Param_Index);
            end if;
         end Get_Value;

      begin
         loop
            declare
               Instruction : constant Integer := Program.Element (Index);
               Opcode      : constant Integer := Instruction rem 100;
               Mode1       : constant Integer := (Instruction / 100) rem 10;
               Mode2       : constant Integer := (Instruction / 1000) rem 10;
               Val1, Val2, Dest : Integer;
            begin
               case Opcode is
                  when 99 =>
                     return;
                  when 1 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     Dest := Program.Element (Index + 3);
                     Program.Replace_Element (Dest, Val1 + Val2);
                     Index := Index + 4;
                  when 2 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     Dest := Program.Element (Index + 3);
                     Program.Replace_Element (Dest, Val1 * Val2);
                     Index := Index + 4;
                  when 3 =>
                     Dest := Program.Element (Index + 1);
                     Program.Replace_Element (Dest, Input_Val);
                     Index := Index + 2;
                  when 4 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Ada.Integer_Text_IO.Put (Item => Val1, Width => 1);
                     Ada.Text_IO.New_Line;
                     Index := Index + 2;
                  when 5 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     if Val1 /= 0 then
                        Index := Val2;
                     else
                        Index := Index + 3;
                     end if;
                  when 6 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     if Val1 = 0 then
                        Index := Val2;
                     else
                        Index := Index + 3;
                     end if;
                  when 7 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     Dest := Program.Element (Index + 3);
                     if Val1 < Val2 then
                        Program.Replace_Element (Dest, 1);
                     else
                        Program.Replace_Element (Dest, 0);
                     end if;
                     Index := Index + 4;
                  when 8 =>
                     Val1 := Get_Value (Index + 1, Mode1);
                     Val2 := Get_Value (Index + 2, Mode2);
                     Dest := Program.Element (Index + 3);
                     if Val1 = Val2 then
                        Program.Replace_Element (Dest, 1);
                     else
                        Program.Replace_Element (Dest, 0);
                     end if;
                     Index := Index + 4;
                  when others =>
                     return;
               end case;
            end;
         end loop;
      end Run_Program;
   begin
      Run_Program (Instructions, 5);
   end;
end Main;
