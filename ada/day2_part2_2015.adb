
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   File         : Ada.Text_IO.File_Type;
   Total_Ribbon : Natural := 0;
begin
   Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Line   : constant String := Ada.Text_IO.Get_Line (File);
         X1_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, "x");
         X2_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, "x", Going => Ada.Strings.Backward);

         L : constant Natural := Integer'Value (Line (Line'First .. X1_Pos - 1));
         W : constant Natural := Integer'Value (Line (X1_Pos + 1 .. X2_Pos - 1));
         H : constant Natural := Integer'Value (Line (X2_Pos + 1 .. Line'Last));

         Bow     : constant Natural := L * W * H;
         Max_Dim : constant Natural := Natural'Max (Natural'Max (L, W), H);
         Wrap    : constant Natural := 2 * (L + W + H - Max_Dim);
      begin
         Total_Ribbon := Total_Ribbon + Bow + Wrap;
      end;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Total_Ribbon);
   Ada.Text_IO.New_Line;
end Main;
