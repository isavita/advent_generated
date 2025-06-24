
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   function Min (A, B, C : Integer) return Integer is
      M : Integer := A;
   begin
      if B < M then
         M := B;
      end if;
      if C < M then
         M := C;
      end if;
      return M;
   end Min;

   File  : Ada.Text_IO.File_Type;
   Total : Natural := 0;
   Line  : String (1 .. 80);
   Last  : Natural;

begin
   Ada.Text_IO.Open (File => File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, Line, Last);
      declare
         Sub_Line : constant String := Line (1 .. Last);
         Pos1     : constant Natural := Ada.Strings.Fixed.Index (Sub_Line, "x");
         Pos2     : constant Natural := Ada.Strings.Fixed.Index (Sub_Line, "x", From => Pos1 + 1);

         L : constant Integer := Integer'Value (Sub_Line (1 .. Pos1 - 1));
         W : constant Integer := Integer'Value (Sub_Line (Pos1 + 1 .. Pos2 - 1));
         H : constant Integer := Integer'Value (Sub_Line (Pos2 + 1 .. Sub_Line'Last));

         Side1 : constant Integer := L * W;
         Side2 : constant Integer := W * H;
         Side3 : constant Integer := H * L;
      begin
         Total := Total + 2 * (Side1 + Side2 + Side3) + Min (Side1, Side2, Side3);
      end;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => Total, Width => 1);
   Ada.Text_IO.New_Line;
end Main;
