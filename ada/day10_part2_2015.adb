
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   File         : File_Type;
   Sequence     : Unbounded_String;
   New_Sequence : Unbounded_String;
   I            : Positive;
   Count        : Natural;
   Current_Char : Character;

begin
   Open (File, In_File, "input.txt");
   Sequence := To_Unbounded_String (Get_Line (File));
   Close (File);

   for Iteration in 1 .. 50 loop
      New_Sequence := Null_Unbounded_String;
      I            := 1;
      while I <= Length (Sequence) loop
         Current_Char := Element (Sequence, I);
         Count        := 1;
         while I + 1 <= Length (Sequence) and then Element (Sequence, I + 1) = Current_Char loop
            Count := Count + 1;
            I     := I + 1;
         end loop;

         Append (New_Sequence, Ada.Strings.Fixed.Trim (Integer'Image (Count), Ada.Strings.Both));
         Append (New_Sequence, Current_Char);

         I := I + 1;
      end loop;
      Sequence := New_Sequence;

      if Iteration = 40 then
         Put_Line (Integer'Image (Length (Sequence)));
      end if;
   end loop;

   Put_Line (Integer'Image (Length (Sequence)));

end Main;
