
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

procedure Main is
   use Ada.Strings.Unbounded;

   function Look_And_Say (Input : in Unbounded_String) return Unbounded_String is
      Result    : Unbounded_String;
      Input_Str : constant String := To_String (Input);
      I         : Positive      := Input_Str'First;
   begin
      if Input_Str'Length = 0 then
         return Null_Unbounded_String;
      end if;

      while I <= Input_Str'Last loop
         declare
            Current_Char : constant Character := Input_Str (I);
            Count        : Positive      := 1;
         begin
            while I < Input_Str'Last and then Input_Str (I + 1) = Current_Char loop
               Count := Count + 1;
               I     := I + 1;
            end loop;
            Append (Result, Ada.Strings.Fixed.Trim (Positive'Image (Count), Ada.Strings.Left));
            Append (Result, Current_Char);
         end;
         I := I + 1;
      end loop;
      return Result;
   end Look_And_Say;

   File : Ada.Text_IO.File_Type;
   Data : Unbounded_String;

begin
   Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   Data := To_Unbounded_String (Ada.Text_IO.Get_Line (File));
   Ada.Text_IO.Close (File);

   for I in 1 .. 40 loop
      Data := Look_And_Say (Data);
   end loop;

   Ada.Integer_Text_IO.Put (Item => Length (Data), Width => 0);
   Ada.Text_IO.New_Line;

end Main;
