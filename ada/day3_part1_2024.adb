
with Ada.Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

procedure Main is
   File      : Ada.Text_IO.File_Type;
   Total_Sum : Long_Integer := 0;
   File_Name : constant String := "input.txt";

   procedure Process_Line (Line : in String) is
      Current_Pos : Positive := Line'First;
   begin
      while Current_Pos <= Line'Last loop
         declare
            Match_Start : constant Natural := Ada.Strings.Fixed.Index
              (Line, "mul", Current_Pos);
         begin
            if Match_Start = 0 then
               exit;
            end if;

            declare
               Parse_Pos : Positive := Match_Start + 3;
               Num1, Num2 : Integer;

               procedure Skip_Whitespace is
               begin
                  while Parse_Pos <= Line'Last and then
                    Ada.Characters.Handling.Is_Space (Line (Parse_Pos))
                  loop
                     Parse_Pos := Parse_Pos + 1;
                  end loop;
               end Skip_Whitespace;

               function Read_Number return Integer is
                  Num_Start : constant Positive := Parse_Pos;
               begin
                  while Parse_Pos <= Line'Last and then
                    Ada.Characters.Handling.Is_Digit (Line (Parse_Pos))
                  loop
                     Parse_Pos := Parse_Pos + 1;
                  end loop;
                  if Num_Start >= Parse_Pos then
                     raise Program_Error;
                  end if;
                  return Integer'Value (Line (Num_Start .. Parse_Pos - 1));
               end Read_Number;

            begin
               Skip_Whitespace;
               if Parse_Pos > Line'Last or else Line (Parse_Pos) /= '(' then
                  raise Program_Error;
               end if;
               Parse_Pos := Parse_Pos + 1;

               Skip_Whitespace;
               Num1 := Read_Number;

               Skip_Whitespace;
               if Parse_Pos > Line'Last or else Line (Parse_Pos) /= ',' then
                  raise Program_Error;
               end if;
               Parse_Pos := Parse_Pos + 1;

               Skip_Whitespace;
               Num2 := Read_Number;

               Skip_Whitespace;
               if Parse_Pos > Line'Last or else Line (Parse_Pos) /= ')' then
                  raise Program_Error;
               end if;

               Total_Sum := Total_Sum + Long_Integer (Num1) * Long_Integer (Num2);
               Current_Pos := Parse_Pos + 1;

            exception
               when Program_Error | Constraint_Error =>
                  Current_Pos := Match_Start + 1;
            end;
         end;
      end loop;
   end Process_Line;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);
   while not Ada.Text_IO.End_Of_File (File) loop
      Process_Line (Ada.Text_IO.Get_Line (File));
   end loop;
   Ada.Text_IO.Close (File);

   Ada.Text_IO.Put ("Total sum: ");
   Ada.Long_Integer_Text_IO.Put (Item => Total_Sum, Width => 1);
   Ada.Text_IO.New_Line;
exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Error: input.txt not found.");
end Main;
