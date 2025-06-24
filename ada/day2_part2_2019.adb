
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;

procedure Main is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);
   use Int_Vectors;

   function Run_Intcode
     (Noun, Verb : Integer; Program : Vector) return Integer
   is
      Memory : Vector  := Program;
      Ip     : Natural := 0;
   begin
      Memory.Replace_Element (1, Noun);
      Memory.Replace_Element (2, Verb);

      while Memory.Element (Ip) /= 99 loop
         declare
            Op    : constant Integer := Memory.Element (Ip);
            Addr1 : constant Natural := Memory.Element (Ip + 1);
            Addr2 : constant Natural := Memory.Element (Ip + 2);
            Dest  : constant Natural := Memory.Element (Ip + 3);
         begin
            if Op = 1 then
               Memory.Replace_Element
                 (Dest, Memory.Element (Addr1) + Memory.Element (Addr2));
            elsif Op = 2 then
               Memory.Replace_Element
                 (Dest, Memory.Element (Addr1) * Memory.Element (Addr2));
            end if;
         end;
         Ip := Ip + 4;
      end loop;

      return Memory.Element (0);
   end Run_Intcode;

   File            : Ada.Text_IO.File_Type;
   Initial_Program : Vector;
   Target_Output   : constant := 19690720;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   declare
      Line      : constant String := Ada.Text_IO.Get_Line (File);
      Start_Idx : Natural         := Line'First;
   begin
      for I in Line'Range loop
         if Line (I) = ',' then
            Initial_Program.Append (Integer'Value (Line (Start_Idx .. I - 1)));
            Start_Idx := I + 1;
         end if;
      end loop;
      Initial_Program.Append (Integer'Value (Line (Start_Idx .. Line'Last)));
   end;
   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Run_Intcode (12, 2, Initial_Program), 1);
   Ada.Text_IO.New_Line;

   Find_Noun_Verb : for Noun in 0 .. 99 loop
      for Verb in 0 .. 99 loop
         if Run_Intcode (Noun, Verb, Initial_Program) = Target_Output then
            Ada.Integer_Text_IO.Put (100 * Noun + Verb, 1);
            Ada.Text_IO.New_Line;
            exit Find_Noun_Verb;
         end if;
      end loop;
   end loop Find_Noun_Verb;

end Main;
