
with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

procedure Main is
   function Hash (Key : Integer) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   package Integer_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => Integer,
      Element_Type    => Natural,
      Hash            => Hash,
      Equivalent_Keys => "="
   );

   package Integer_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Natural,
      Element_Type => Integer
   );

   File              : Ada.Text_IO.File_Type;
   Location_IDs      : Integer_Vectors.Vector;
   Similarity_Counts : Integer_Maps.Map;
   Total             : Long_Long_Integer := 0;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Location_ID : Integer;
         Similarity  : Integer;
      begin
         declare
            package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
         begin
            Int_IO.Get (File, Location_ID);
            Int_IO.Get (File, Similarity);
         end;

         Location_IDs.Append (Location_ID);
         declare
            Cursor : constant Integer_Maps.Cursor := Similarity_Counts.Find (Similarity);
         begin
            if Integer_Maps.Has_Element (Cursor) then
               Similarity_Counts.Replace_Element (Cursor, Integer_Maps.Element (Cursor) + 1);
            else
               Similarity_Counts.Insert (Similarity, 1);
            end if;
         end;

      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line (File);
      end;
   end loop;

   Ada.Text_IO.Close (File);

   for ID of Location_IDs loop
      if Similarity_Counts.Contains (ID) then
         Total := Total + Long_Long_Integer (ID) * Long_Long_Integer (Similarity_Counts.Element (ID));
      end if;
   end loop;

   declare
      package LLI_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   begin
      LLI_IO.Put (Item => Total, Width => 1);
      Ada.Text_IO.New_Line;
   end;

end Main;
