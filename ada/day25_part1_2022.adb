
with Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Main is

   function From_Snafu (S : in String) return Long_Long_Integer is
      N : Long_Long_Integer := 0;
   begin
      for I in S'Range loop
         N := N * 5;
         case S(I) is
            when '=' =>
               N := N - 2;
            when '-' =>
               N := N - 1;
            when '0' | '1' | '2' =>
               N := N + Long_Long_Integer(Character'Pos(S(I)) - Character'Pos('0'));
            when others =>
               null;
         end case;
      end loop;
      return N;
   end From_Snafu;

   function To_Snafu (N_Val : in Long_Long_Integer) return Unbounded_String is
      Result_Chars : Unbounded_String;
      Current_N    : Long_Long_Integer := N_Val;
   begin
      while Current_N > 0 loop
         declare
            Remainder : Long_Long_Integer := Current_N mod 5;
         begin
            case Remainder is
               when 3 =>
                  Current_N := Current_N + 5;
                  Append(Result_Chars, '=');
               when 4 =>
                  Current_N := Current_N + 5;
                  Append(Result_Chars, '-');
               when others =>
                  Append(Result_Chars, Character'Val(Character'Pos('0') + Integer(Remainder)));
            end case;
            Current_N := Current_N / 5;
         end;
      end loop;

      declare
         S_Len     : Natural := Length(Result_Chars);
         Temp_Char : Character;
      begin
         for I in 1 .. S_Len / 2 loop
            Temp_Char := Element(Result_Chars, I);
            Replace_Element(Result_Chars, I, Element(Result_Chars, S_Len - I + 1));
            Replace_Element(Result_Chars, S_Len - I + 1, Temp_Char);
         end loop;
      end;

      return Result_Chars;
   end To_Snafu;

   Sum_Val    : Long_Long_Integer := 0;
   Input_File : Ada.Text_IO.File_Type;
   Line       : String(1 .. 256);
   Last       : Natural;

begin
   Ada.Text_IO.Open(Input_File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File(Input_File) loop
      Ada.Text_IO.Get_Line(Input_File, Line, Last);
      Sum_Val := Sum_Val + From_Snafu(Line(1 .. Last));
   end loop;

   Ada.Text_IO.Close(Input_File);

   Ada.Text_IO.Put_Line(To_String(To_Snafu(Sum_Val)));

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line("Error: input.txt not found.");
   when others =>
      Ada.Text_IO.Put_Line("An unexpected error occurred.");

end Main;
