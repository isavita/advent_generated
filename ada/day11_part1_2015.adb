
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.IO_Exceptions;

procedure Main is

   function Read_Input (Filename : String) return Ada.Strings.Unbounded.Unbounded_String is
      File : Ada.Text_IO.File_Type;
      Line : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
      begin
         Ada.Strings.Unbounded.Text_IO.Get_Line (File, Line);
         Ada.Text_IO.Close (File);
         return Line;
      exception
         when Ada.IO_Exceptions.End_Error =>
            Ada.Text_IO.Close (File);
            return Ada.Strings.Unbounded.Null_Unbounded_String;
      end;
   exception
      when Ada.Text_IO.Name_Error =>
         raise;
   end Read_Input;

   function Increment_Password (Password : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String is
      Result_String : Ada.Strings.Unbounded.Unbounded_String := Password;
      Len           : Natural := Ada.Strings.Unbounded.Length (Result_String);
      Carry         : Boolean := True;
   begin
      if Len = 0 then
         return Ada.Strings.Unbounded.To_Unbounded_String ("a");
      end if;

      for I in reverse 1 .. Len loop
         if Carry then
            declare
               Char_Val : Character := Ada.Strings.Unbounded.Element (Result_String, I);
            begin
               if Char_Val = 'z' then
                  Ada.Strings.Unbounded.Replace_Element (Result_String, I, 'a');
                  Carry := True;
               else
                  Ada.Strings.Unbounded.Replace_Element (Result_String, I, Character'Val(Character'Pos(Char_Val) + 1));
                  Carry := False;
               end if;

               Char_Val := Ada.Strings.Unbounded.Element (Result_String, I);
               if Char_Val = 'i' or else Char_Val = 'o' or else Char_Val = 'l' then
                  Ada.Strings.Unbounded.Replace_Element (Result_String, I, Character'Val(Character'Pos(Char_Val) + 1));
                  Carry := False;
                  for J in I + 1 .. Len loop
                     Ada.Strings.Unbounded.Replace_Element (Result_String, J, 'a');
                  end loop;
                  exit;
               end if;
            end;
         end if;
      end loop;
      return Result_String;
   end Increment_Password;

   function Has_Straight (Password : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
      Len : Natural := Ada.Strings.Unbounded.Length (Password);
   begin
      if Len < 3 then
         return False;
      end if;
      for I in 1 .. Len - 2 loop
         declare
            C1 : Character := Ada.Strings.Unbounded.Element (Password, I);
            C2 : Character := Ada.Strings.Unbounded.Element (Password, I + 1);
            C3 : Character := Ada.Strings.Unbounded.Element (Password, I + 2);
         begin
            if Character'Pos(C1) + 1 = Character'Pos(C2) and then
               Character'Pos(C1) + 2 = Character'Pos(C3)
            then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Has_Straight;

   function Contains_Invalid_Letters (Password : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
      Len : Natural := Ada.Strings.Unbounded.Length (Password);
   begin
      for I in 1 .. Len loop
         declare
            C : Character := Ada.Strings.Unbounded.Element (Password, I);
         begin
            if C = 'i' or else C = 'o' or else C = 'l' then
               return True;
            end if;
         end;
      end loop;
      return False;
   end Contains_Invalid_Letters;

   function Has_Two_Pairs (Password : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
      Count : Natural := 0;
      Len   : Natural := Ada.Strings.Unbounded.Length (Password);
      I     : Natural := 1;
   begin
      while I <= Len - 1 loop
         declare
            C1 : Character := Ada.Strings.Unbounded.Element (Password, I);
            C2 : Character := Ada.Strings.Unbounded.Element (Password, I + 1);
         begin
            if C1 = C2 then
               Count := Count + 1;
               I := I + 2;
            else
               I := I + 1;
            end if;
         end;
      end loop;
      return Count >= 2;
   end Has_Two_Pairs;

   function Is_Valid_Password (Password : Ada.Strings.Unbounded.Unbounded_String) return Boolean is
   begin
      return Has_Straight (Password) and then
             not Contains_Invalid_Letters (Password) and then
             Has_Two_Pairs (Password);
   end Is_Valid_Password;

   function Find_Next_Password (Password : Ada.Strings.Unbounded.Unbounded_String)
      return Ada.Strings.Unbounded.Unbounded_String is
      Current_Attempt : Ada.Strings.Unbounded.Unbounded_String := Password;
   begin
      loop
         Current_Attempt := Increment_Password (Current_Attempt);
         exit when Is_Valid_Password (Current_Attempt);
      end loop;
      return Current_Attempt;
   end Find_Next_Password;

   Current_Password : Ada.Strings.Unbounded.Unbounded_String;
   New_Password     : Ada.Strings.Unbounded.Unbounded_String;

begin
   begin
      Current_Password := Read_Input ("input.txt");
      if Ada.Strings.Unbounded.Length(Current_Password) = 0 then
         Ada.Text_IO.Put_Line ("Error: input.txt is empty.");
         return;
      end if;
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("Error: input.txt not found.");
         return;
      when others =>
         Ada.Text_IO.Put_Line ("An unexpected error occurred during file reading.");
         return;
   end;

   New_Password := Find_Next_Password (Current_Password);

   Ada.Strings.Unbounded.Text_IO.Put_Line (New_Password);

end Main;
