
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Disk_Length : constant := 272;

   function Dragon_Curve (A : Unbounded_String) return Unbounded_String is
      Result : Unbounded_String := A;
   begin
      Append (Result, '0');
      for I in reverse 1 .. Length (A) loop
         if Element (A, I) = '0' then
            Append (Result, '1');
         else
            Append (Result, '0');
         end if;
      end loop;
      return Result;
   end Dragon_Curve;

   function Calculate_Checksum (Data : String) return String is
      Current_Data : Unbounded_String := To_Unbounded_String (Data);
      Next_Data    : Unbounded_String;
   begin
      while Length (Current_Data) mod 2 = 0 loop
         Next_Data := Null_Unbounded_String;
         for I in 1 .. Length (Current_Data) / 2 loop
            if Element (Current_Data, 2 * I - 1) = Element (Current_Data, 2 * I) then
               Append (Next_Data, '1');
            else
               Append (Next_Data, '0');
            end if;
         end loop;
         Current_Data := Next_Data;
      end loop;
      return To_String (Current_Data);
   end Calculate_Checksum;

   File : File_Type;
   Data : Unbounded_String;
begin
   Open (File, In_File, "input.txt");
   Data := To_Unbounded_String (Get_Line (File));
   Close (File);

   while Length (Data) < Disk_Length loop
      Data := Dragon_Curve (Data);
   end loop;

   Put_Line (Calculate_Checksum (To_String (Data) (1 .. Disk_Length)));

end Main;
