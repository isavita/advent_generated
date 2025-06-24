
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Integer_Text_IO;

procedure Main is
   subtype Number_Range is Natural range 0 .. 99;
   type Number_Set is array (Number_Range) of Boolean;

   type Card is record
      Matches    : Natural := 0;
      Copy_Count : Positive := 1;
   end record;

   package Card_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Card);
   use Card_Vectors;

   File        : Ada.Text_IO.File_Type;
   Cards       : Vector;
   Line_Buffer : String (1 .. 256);
   Line_Length : Natural;

begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, Line_Buffer, Line_Length);
      if Line_Length > 0 then
         declare
            Line         : constant String := Line_Buffer (1 .. Line_Length);
            Colon_Pos    : constant Natural := Ada.Strings.Fixed.Index (Line, ":");
            Bar_Pos      : constant Natural := Ada.Strings.Fixed.Index (Line, "|");
            Winnings_Str : constant String := Line (Colon_Pos + 2 .. Bar_Pos - 2);
            Givens_Str   : constant String := Line (Bar_Pos + 2 .. Line'Last);
            Winnings     : Number_Set := (others => False);
            Match_Count  : Natural := 0;
            Num_Str      : String (1 .. 2) := "  ";
            Num_Str_Len  : Natural := 0;
         begin
            for C of Winnings_Str loop
               if Ada.Characters.Handling.Is_Digit (C) then
                  Num_Str_Len := Num_Str_Len + 1;
                  Num_Str (Num_Str_Len) := C;
               elsif Num_Str_Len > 0 then
                  Winnings (Integer'Value (Num_Str (1 .. Num_Str_Len))) := True;
                  Num_Str_Len := 0;
               end if;
            end loop;
            if Num_Str_Len > 0 then
               Winnings (Integer'Value (Num_Str (1 .. Num_Str_Len))) := True;
               Num_Str_Len := 0;
            end if;

            for C of Givens_Str loop
               if Ada.Characters.Handling.Is_Digit (C) then
                  Num_Str_Len := Num_Str_Len + 1;
                  Num_Str (Num_Str_Len) := C;
               elsif Num_Str_Len > 0 then
                  if Winnings (Integer'Value (Num_Str (1 .. Num_Str_Len))) then
                     Match_Count := Match_Count + 1;
                  end if;
                  Num_Str_Len := 0;
               end if;
            end loop;
            if Num_Str_Len > 0 then
               if Winnings (Integer'Value (Num_Str (1 .. Num_Str_Len))) then
                  Match_Count := Match_Count + 1;
               end if;
            end if;

            Cards.Append (New_Item => (Matches => Match_Count, Copy_Count => 1));
         end;
      end if;
   end loop;

   Ada.Text_IO.Close (File);

   for I in Cards.First_Index .. Cards.Last_Index loop
      declare
         Current_Card : constant Card := Cards.Element (I);
      begin
         for J in 1 .. Current_Card.Matches loop
            declare
               Target_Index : constant Positive := I + J;
            begin
               if Target_Index <= Cards.Last_Index then
                  declare
                     Target_Card : Card := Cards.Element (Target_Index);
                  begin
                     Target_Card.Copy_Count :=
                       Target_Card.Copy_Count + Current_Card.Copy_Count;
                     Cards.Replace_Element (Target_Index, Target_Card);
                  end;
               end if;
            end;
         end loop;
      end;
   end loop;

   declare
      Total_Cards : Natural := 0;
   begin
      for C of Cards loop
         Total_Cards := Total_Cards + C.Copy_Count;
      end loop;
      Ada.Integer_Text_IO.Put (Item => Total_Cards, Width => 0);
      Ada.Text_IO.New_Line;
   end;
end Main;
