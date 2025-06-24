
with Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Containers.Vectors;

procedure Main is
   package Char_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Character);

   File  : Ada.Text_IO.File_Type;
   Stack : Char_Vectors.Vector;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   declare
      Polymer : constant String := Ada.Text_IO.Get_Line (File);
   begin
      for Unit of Polymer loop
         if not Stack.Is_Empty and then
           Unit /= Stack.Last_Element and then
           Ada.Characters.Handling.To_Lower (Unit) =
           Ada.Characters.Handling.To_Lower (Stack.Last_Element)
         then
            Stack.Delete_Last;
         else
            Stack.Append (Unit);
         end if;
      end loop;
   end;
   Ada.Text_IO.Close (File);

   Ada.Text_IO.Put_Line (Stack.Length'Image);

exception
   when Ada.Text_IO.End_Error =>
      Ada.Text_IO.Put_Line (Stack.Length'Image);
end Main;
