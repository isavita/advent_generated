
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

procedure Main is
   type Ingredient is record
      Capacity   : Integer;
      Durability : Integer;
      Flavor     : Integer;
      Texture    : Integer;
      Calories   : Integer;
   end record;

   package Ingredient_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Ingredient);

   function Read_Ingredients (Filename : String) return Ingredient_Vectors.Vector is
      File        : Ada.Text_IO.File_Type;
      Ingredients : Ingredient_Vectors.Vector;
   begin
      Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => Filename);
      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            Line         : constant String := Ada.Text_IO.Get_Line (File);
            Words        : array (1 .. 11) of Ada.Strings.Unbounded.Unbounded_String;
            Current_Word : Ada.Strings.Unbounded.Unbounded_String;
            Word_Index   : Positive := 1;
         begin
            for C of Line loop
               if C = ' ' then
                  if Ada.Strings.Unbounded.Length (Current_Word) > 0 then
                     if Word_Index <= Words'Last then
                        Words (Word_Index) := Current_Word;
                        Word_Index       := Word_Index + 1;
                     end if;
                     Current_Word := Ada.Strings.Unbounded.Null_Unbounded_String;
                  end if;
               else
                  Ada.Strings.Unbounded.Append (Current_Word, C);
               end if;
            end loop;
            if Ada.Strings.Unbounded.Length (Current_Word) > 0 and then Word_Index <= Words'Last then
               Words (Word_Index) := Current_Word;
            end if;

            declare
               Cap_Str : constant String := Ada.Strings.Unbounded.To_String (Words (3));
               Dur_Str : constant String := Ada.Strings.Unbounded.To_String (Words (5));
               Fla_Str : constant String := Ada.Strings.Unbounded.To_String (Words (7));
               Tex_Str : constant String := Ada.Strings.Unbounded.To_String (Words (9));
               Cal_Str : constant String := Ada.Strings.Unbounded.To_String (Words (11));
            begin
               Ingredients.Append
                 ((Capacity   => Integer'Value (Cap_Str (Cap_Str'First .. Cap_Str'Last - 1)),
                   Durability => Integer'Value (Dur_Str (Dur_Str'First .. Dur_Str'Last - 1)),
                   Flavor     => Integer'Value (Fla_Str (Fla_Str'First .. Fla_Str'Last - 1)),
                   Texture    => Integer'Value (Tex_Str (Tex_Str'First .. Tex_Str'Last - 1)),
                   Calories   => Integer'Value (Cal_Str)));
            end;
         end;
      end loop;
      Ada.Text_IO.Close (File);
      return Ingredients;
   end Read_Ingredients;

   Ingredients     : constant Ingredient_Vectors.Vector := Read_Ingredients ("input.txt");
   Max_Score       : Natural := 0;
   Total_Teaspoons : constant := 100;
   Target_Calories : constant := 500;

begin
   for I1 in 0 .. Total_Teaspoons loop
      for I2 in 0 .. Total_Teaspoons - I1 loop
         for I3 in 0 .. Total_Teaspoons - I1 - I2 loop
            declare
               I4 : constant Integer := Total_Teaspoons - I1 - I2 - I3;
               Teaspoons : constant array (Positive range 1 .. 4) of Integer := (I1, I2, I3, I4);
               Total_Calories : Integer := 0;
               Capacity, Durability, Flavor, Texture : Integer := 0;
            begin
               for I in Teaspoons'Range loop
                  Total_Calories := Total_Calories + Ingredients.Element (I).Calories * Teaspoons (I);
                  Capacity       := Capacity + Ingredients.Element (I).Capacity * Teaspoons (I);
                  Durability     := Durability + Ingredients.Element (I).Durability * Teaspoons (I);
                  Flavor         := Flavor + Ingredients.Element (I).Flavor * Teaspoons (I);
                  Texture        := Texture + Ingredients.Element (I).Texture * Teaspoons (I);
               end loop;

               if Total_Calories = Target_Calories then
                  if Capacity < 0 then Capacity := 0; end if;
                  if Durability < 0 then Durability := 0; end if;
                  if Flavor < 0 then Flavor := 0; end if;
                  if Texture < 0 then Texture := 0; end if;

                  declare
                     Current_Score : constant Natural := Natural (Capacity) * Natural (Durability) * Natural (Flavor) * Natural (Texture);
                  begin
                     if Current_Score > Max_Score then
                        Max_Score := Current_Score;
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;
   end loop;

   Ada.Integer_Text_IO.Put (Max_Score);
   Ada.Text_IO.New_Line;

end Main;
