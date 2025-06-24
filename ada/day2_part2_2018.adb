
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unbounded_String);
   use String_Vectors;

   File  : File_Type;
   Lines : Vector;

begin
   Open (File, In_File, "input.txt");
   while not End_Of_File (File) loop
      Lines.Append (To_Unbounded_String (Get_Line (File)));
   end loop;
   Close (File);

   for I in Lines.First_Index .. Lines.Last_Index - 1 loop
      for J in I + 1 .. Lines.Last_Index loop
         declare
            Line1 : constant Unbounded_String := Lines.Element (I);
            Line2 : constant Unbounded_String := Lines.Element (J);
            Diff  : Natural := 0;
         begin
            for K in 1 .. Length (Line1) loop
               if Element (Line1, K) /= Element (Line2, K) then
                  Diff := Diff + 1;
               end if;
               if Diff > 1 then
                  exit;
               end if;
            end loop;

            if Diff = 1 then
               declare
                  Common : Unbounded_String := Null_Unbounded_String;
               begin
                  for K in 1 .. Length (Line1) loop
                     if Element (Line1, K) = Element (Line2, K) then
                        Append (Common, Element (Line1, K));
                     end if;
                  end loop;
                  Put_Line (To_String (Common));
                  return;
               end;
            end if;
         end;
      end loop;
   end loop;
end Main;
