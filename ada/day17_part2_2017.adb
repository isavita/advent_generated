
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;

procedure Spinlock is
   File_Handle : File_Type;
   Input_String : Unbounded_String;
   Steps : Integer;
   Current_Pos : Integer := 0;
   Value_After_Zero : Integer := 0;
   I : Integer;
begin
   Open(File_Handle, In_File, "input.txt");
   Input_String := To_Unbounded_String(Get_Line(File_Handle));
   Close(File_Handle);

   Steps := Integer'Value(To_String(Input_String));

   for I in 1 .. 50_000_000 loop
      Current_Pos := (Current_Pos + Steps) mod I;
      if Current_Pos = 0 then
         Value_After_Zero := I;
      end if;
      Current_Pos := Current_Pos + 1;
   end loop;

   Put(Value_After_Zero);
   New_Line;
end Spinlock;
