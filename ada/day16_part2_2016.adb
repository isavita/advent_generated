
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Dragon_Checksum is

   Disk_Length : constant Natural := 35_651_584;

   function Read_Initial_State (Filename : String) return Ada.Strings.Unbounded.Unbounded_String is
      File        : Ada.Text_IO.File_Type;
      Result_Line : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
      if not Ada.Text_IO.End_Of_File (File) then
         Result_Line := Ada.Strings.Unbounded.To_Unbounded_String (Ada.Text_IO.Get_Line (File));
      else
         Ada.Text_IO.Close (File);
         raise Program_Error with "Failed to read initial state: file is empty or no line found";
      end if;
      Ada.Text_IO.Close (File);
      return Result_Line;
   exception
      when Ada.Text_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("Error: File not found: " & Filename);
         raise;
      when others =>
         Ada.Text_IO.Put_Line ("An unexpected error occurred while reading file: " & Filename);
         raise;
   end Read_Initial_State;

   function Generate_Data (Initial_State : Ada.Strings.Unbounded.Unbounded_String; Length : Natural) return Ada.Strings.Unbounded.Unbounded_String is
      Data           : Ada.Strings.Unbounded.Unbounded_String := Initial_State;
      Current_Length : Natural;
      B_Builder      : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         Current_Length := Ada.Strings.Unbounded.Length (Data);
         exit when Current_Length >= Length;

         B_Builder := Ada.Strings.Unbounded.Null_Unbounded_String;
         for I in reverse 1 .. Current_Length loop
            if Ada.Strings.Unbounded.Element (Data, I) = '0' then
               Ada.Strings.Unbounded.Append (B_Builder, '1');
            else
               Ada.Strings.Unbounded.Append (B_Builder, '0');
            end if;
         end loop;

         Ada.Strings.Unbounded.Append (Data, '0');
         Ada.Strings.Unbounded.Append (Data, B_Builder);
      end loop;

      return Ada.Strings.Unbounded.To_Unbounded_String (Ada.Strings.Unbounded.To_String (Data) (1 .. Length));
   end Generate_Data;

   function Calculate_Checksum (Data_In : Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      Current_Data     : Ada.Strings.Unbounded.Unbounded_String := Data_In;
      Current_Length   : Natural;
      Checksum_Builder : Ada.Strings.Unbounded.Unbounded_String;
   begin
      loop
         Current_Length := Ada.Strings.Unbounded.Length (Current_Data);
         exit when Current_Length mod 2 /= 0;

         Checksum_Builder := Ada.Strings.Unbounded.Null_Unbounded_String;
         for I in 1 .. Current_Length / 2 loop
            declare
               Char1 : Character := Ada.Strings.Unbounded.Element (Current_Data, 2 * I - 1);
               Char2 : Character := Ada.Strings.Unbounded.Element (Current_Data, 2 * I);
            begin
               if Char1 = Char2 then
                  Ada.Strings.Unbounded.Append (Checksum_Builder, '1');
               else
                  Ada.Strings.Unbounded.Append (Checksum_Builder, '0');
               end if;
            end;
         end loop;
         Current_Data := Checksum_Builder;
      end loop;
      return Current_Data;
   end Calculate_Checksum;

   Initial_State_Str  : Ada.Strings.Unbounded.Unbounded_String;
   Generated_Data_Str : Ada.Strings.Unbounded.Unbounded_String;
   Final_Checksum_Str : Ada.Strings.Unbounded.Unbounded_String;

begin
   Initial_State_Str  := Read_Initial_State ("input.txt");
   Generated_Data_Str := Generate_Data (Initial_State_Str, Disk_Length);
   Final_Checksum_Str := Calculate_Checksum (Generated_Data_Str);
   Ada.Text_IO.Put_Line ("Checksum: " & Ada.Strings.Unbounded.To_String (Final_Checksum_Str));

exception
   when Program_Error =>
      null; -- Error message already printed by the raise statement
   when others =>
      Ada.Text_IO.Put_Line ("An unhandled error occurred.");
end Dragon_Checksum;
