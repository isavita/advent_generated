
with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

procedure Main is
   type Disc is record
      Total_Positions : Natural;
      Start_Position  : Natural;
   end record;

   package Disc_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Disc);
   use Disc_Vectors;

   Discs : Vector;
   File  : Ada.Text_IO.File_Type;
   Input_File_Name : constant String := "input.txt";

begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => Input_File_Name);
   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line    : constant String := Ada.Text_IO.Get_Line (File);
         Pos_Has : constant Natural := Ada.Strings.Fixed.Index (Line, " has ");
         Pos_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, " positions;");
         Pos_At  : constant Natural := Ada.Strings.Fixed.Index (Line, " position ");
         Pos_End : constant Natural := Ada.Strings.Fixed.Index (Line, ".");
      begin
         Discs.Append ((
            Total_Positions => Natural'Value (Line (Pos_Has + 5 .. Pos_Pos - 1)),
            Start_Position  => Natural'Value (Line (Pos_At + 10 .. Pos_End - 1))
         ));
      end;
   end loop;
   Ada.Text_IO.Close (File);

   Discs.Append ((Total_Positions => 11, Start_Position => 0));

   declare
      Time : Long_Long_Integer := 0;
      Step : Long_Long_Integer := 1;
   begin
      for I in Discs.First_Index .. Discs.Last_Index loop
         declare
            D   : constant Disc := Discs.Element (I);
            M   : constant Long_Long_Integer := Long_Long_Integer(D.Total_Positions);
            S   : constant Long_Long_Integer := Long_Long_Integer(D.Start_Position);
            Idx : constant Long_Long_Integer := Long_Long_Integer(I);
         begin
            while (S + Time + Idx) mod M /= 0 loop
               Time := Time + Step;
            end loop;
            Step := Step * M;
         end;
      end loop;

      Ada.Long_Long_Integer_Text_IO.Put (Item => Time, Width => 1);
      Ada.Text_IO.New_Line;
   end;
end Main;
