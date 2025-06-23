
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

procedure Main is
   type Coordinate is record
      X, Y : Integer;
   end record;

   function Hash (Key : Coordinate) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (Key.X * 10007 + Key.Y);
   end Hash;

   function Equivalent_Keys (L, R : Coordinate) return Boolean is
     (L.X = R.X and L.Y = R.Y);

   package Fabric_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Coordinate,
      Element_Type    => Natural,
      Hash            => Hash,
      Equivalent_Keys => Equivalent_Keys);

   type Claim is record
      ID                  : String (1 .. 5);
      ID_Len              : Natural;
      X, Y, Width, Height : Integer;
   end record;

   package Claim_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Claim);

   Fabric : Fabric_Maps.Map;
   Claims : Claim_Vectors.Vector;
   File   : Ada.Text_IO.File_Type;
   Line   : String (1 .. 256);
   Length : Natural;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      Ada.Text_IO.Get_Line (File, Line, Length);
      declare
         Line_Str  : constant String := Line (1 .. Length);
         At_Pos    : constant Natural := Ada.Strings.Fixed.Index (Line_Str, "@");
         Comma_Pos : constant Natural := Ada.Strings.Fixed.Index (Line_Str, ",");
         Colon_Pos : constant Natural := Ada.Strings.Fixed.Index (Line_Str, ":");
         X_Pos     : constant Natural := Ada.Strings.Fixed.Index (Line_Str, "x");
         New_Claim : Claim;
      begin
         New_Claim.ID := (others => ' ');
         New_Claim.ID (1 .. At_Pos - 2) := Line_Str (1 .. At_Pos - 2);
         New_Claim.ID_Len := At_Pos - 2;
         New_Claim.X      := Integer'Value (Line_Str (At_Pos + 2 .. Comma_Pos - 1));
         New_Claim.Y      := Integer'Value (Line_Str (Comma_Pos + 1 .. Colon_Pos - 1));
         New_Claim.Width  := Integer'Value (Line_Str (Colon_Pos + 2 .. X_Pos - 1));
         New_Claim.Height := Integer'Value (Line_Str (X_Pos + 1 .. Line_Str'Last));

         Claims.Append (New_Claim);

         for I in New_Claim.X .. New_Claim.X + New_Claim.Width - 1 loop
            for J in New_Claim.Y .. New_Claim.Y + New_Claim.Height - 1 loop
               declare
                  Pos    : constant Coordinate := (X => I, Y => J);
                  Cursor : Fabric_Maps.Cursor := Fabric.Find (Pos);
               begin
                  if Fabric_Maps.Has_Element (Cursor) then
                     Fabric.Replace_Element (Cursor, Fabric_Maps.Element (Cursor) + 1);
                  else
                     Fabric.Insert (Pos, 1);
                  end if;
               end;
            end loop;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Close (File);

   declare
      Overlapping_Inches : Natural := 0;
   begin
      for E of Fabric loop
         if E > 1 then
            Overlapping_Inches := Overlapping_Inches + 1;
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Overlapping_Inches'Image);
   end;

   Claim_Loop : for C of Claims loop
      declare
         Is_Intact : Boolean := True;
      begin
         Check_Loop : for I in C.X .. C.X + C.Width - 1 loop
            for J in C.Y .. C.Y + C.Height - 1 loop
               if Fabric.Element ((X => I, Y => J)) /= 1 then
                  Is_Intact := False;
                  exit Check_Loop;
               end if;
            end loop;
         end loop Check_Loop;

         if Is_Intact then
            Ada.Text_IO.Put_Line (C.ID (1 .. C.ID_Len));
            exit Claim_Loop;
         end if;
      end;
   end loop Claim_Loop;

end Main;
