
with Ada.Text_IO;

procedure Main is
   package Long_IO is new Ada.Text_IO.Integer_IO (Long_Integer);

   Input     : Ada.Text_IO.File_Type;
   Frequency : Long_Integer := 0;
   Value     : Long_Integer;
begin
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input) loop
      Long_IO.Get (File => Input, Item => Value);
      Frequency := Frequency + Value;
   end loop;

   Ada.Text_IO.Close (Input);

   Ada.Text_IO.Put_Line (Long_Integer'Image (Frequency));
end Main;
