
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   Fabric_Size : constant := 1001;
   type Fabric_Grid is array (0 .. Fabric_Size - 1, 0 .. Fabric_Size - 1) of Natural;
   Fabric : Fabric_Grid := (others => (others => 0));

   Input_File    : Ada.Text_IO.File_Type;
   Overlap_Count : Natural := 0;

   Left_Offset, Top_Offset, Width, Height : Natural;

   procedure Parse_Line (
      Line         : in  String;
      Left_Offset  : out Natural;
      Top_Offset   : out Natural;
      Width        : out Natural;
      Height       : out Natural
   ) is
      At_Pos    : constant Natural := Ada.Strings.Fixed.Index (Line, "@");
      Comma_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, ",");
      Colon_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, ":");
      X_Pos     : constant Natural := Ada.Strings.Fixed.Index (Line, "x");
   begin
      Left_Offset := Natural'Value (Line (At_Pos + 2 .. Comma_Pos - 1));
      Top_Offset  := Natural'Value (Line (Comma_Pos + 1 .. Colon_Pos - 1));
      Width       := Natural'Value (Line (Colon_Pos + 2 .. X_Pos - 1));
      Height      := Natural'Value (Line (X_Pos + 1 .. Line'Last));
   end Parse_Line;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line (Input_File);
      begin
         if Line'Length > 0 then
            Parse_Line (Line, Left_Offset, Top_Offset, Width, Height);

            for I in Left_Offset .. Left_Offset + Width - 1 loop
               for J in Top_Offset .. Top_Offset + Height - 1 loop
                  Fabric (I, J) := Fabric (I, J) + 1;
                  if Fabric (I, J) = 2 then
                     Overlap_Count := Overlap_Count + 1;
                  end if;
               end loop;
            end loop;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Overlap_Count, Width => 0);
   Ada.Text_IO.New_Line;

end Main;
