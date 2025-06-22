
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is

   type LL_Node;
   type LL_Node_Access is access LL_Node;

   type LL_Node is record
      Elf_Num   : Integer;
      Presents  : Integer;
      Next      : LL_Node_Access;
   end record;

   function Read_Input_Value (File_Name : String) return Integer is
      File_Handle : File_Type;
      Value       : Integer;
   begin
      Open (File_Handle, In_File, File_Name);
      Get (File_Handle, Value);
      Close (File_Handle);
      return Value;
   end Read_Input_Value;

   function Elephant (Starting_Elves : Integer) return Integer is
      Root           : LL_Node_Access;
      Iter           : LL_Node_Access;
      Before_Across  : LL_Node_Access;
      Is_Odd_Length  : Boolean;
   begin
      if Starting_Elves = 1 then
         return 1;
      end if;

      Root := new LL_Node'(Elf_Num => 1, Presents => 1, Next => null);
      Iter := Root;

      for I in 2 .. Starting_Elves loop
         Iter.Next := new LL_Node'(Elf_Num => I, Presents => 1, Next => null);
         Iter := Iter.Next;
      end loop;
      Iter.Next := Root;

      Is_Odd_Length := (Starting_Elves mod 2 = 1);
      Before_Across := Root;
      for I in 1 .. (Starting_Elves / 2) - 1 loop
         Before_Across := Before_Across.Next;
      end loop;

      while Root.Next /= Root loop
         Root.Presents := Root.Presents + Before_Across.Next.Presents;
         Before_Across.Next := Before_Across.Next.Next;

         if Is_Odd_Length then
            Before_Across := Before_Across.Next;
         end if;
         Is_Odd_Length := not Is_Odd_Length;
         Root := Root.Next;
      end loop;

      return Root.Elf_Num;
   end Elephant;

   Input_Value : Integer;
   Answer      : Integer;
begin
   Input_Value := Read_Input_Value("input.txt");
   Answer := Elephant(Input_Value);
   Put_Line (Integer'Image(Answer));
end Main;
