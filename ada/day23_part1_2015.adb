
with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;

procedure Solve is
   type Opcode is (Hlf, Tpl, Inc, Jmp, Jie, Jio);
   type Register_Name is (A, B);

   type Instruction is record
      Op  : Opcode;
      Reg : Register_Name := A;
      Off : Integer       := 0;
   end record;

   package Instruction_Vectors is new Ada.Containers.Vectors (Positive, Instruction);

   function Parse_Line (Line : String) return Instruction is
      First_Space : constant Natural := Ada.Strings.Fixed.Index (Line, " ");
      Op_Str      : constant String  := Line (Line'First .. First_Space - 1);
      Rest        : constant String  := Line (First_Space + 1 .. Line'Last);
      Result      : Instruction;
   begin
      if Op_Str = "hlf" or Op_Str = "tpl" or Op_Str = "inc" then
         Result.Reg := (if Rest (Rest'First) = 'a' then A else B);
         if Op_Str = "hlf" then Result.Op := Hlf;
         elsif Op_Str = "tpl" then Result.Op := Tpl;
         else Result.Op := Inc; end if;
      elsif Op_Str = "jmp" then
         Result.Op := Jmp;
         Result.Off := Integer'Value (Rest);
      else
         Result.Reg := (if Rest (Rest'First) = 'a' then A else B);
         if Op_Str = "jie" then Result.Op := Jie;
         else Result.Op := Jio; end if;
         declare
            Comma_Pos : constant Natural := Ada.Strings.Fixed.Index (Rest, ",");
         begin
            Result.Off := Integer'Value (Rest (Comma_Pos + 2 .. Rest'Last));
         end;
      end if;
      return Result;
   end Parse_Line;

   File         : Ada.Text_IO.File_Type;
   Instructions : Instruction_Vectors.Vector;
   Registers    : array (Register_Name) of Long_Long_Integer := (A => 0, B => 0);
   PC           : Integer := 1;

begin
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, "input.txt");
   while not Ada.Text_IO.End_Of_File (File) loop
      Instructions.Append (Parse_Line (Ada.Text_IO.Get_Line (File)));
   end loop;
   Ada.Text_IO.Close (File);

   while PC in Instructions.First_Index .. Instructions.Last_Index loop
      declare
         I : constant Instruction := Instructions (PC);
      begin
         case I.Op is
            when Hlf =>
               Registers (I.Reg) := Registers (I.Reg) / 2;
               PC := PC + 1;
            when Tpl =>
               Registers (I.Reg) := Registers (I.Reg) * 3;
               PC := PC + 1;
            when Inc =>
               Registers (I.Reg) := Registers (I.Reg) + 1;
               PC := PC + 1;
            when Jmp =>
               PC := PC + I.Off;
            when Jie =>
               if Registers (I.Reg) mod 2 = 0 then
                  PC := PC + I.Off;
               else
                  PC := PC + 1;
               end if;
            when Jio =>
               if Registers (I.Reg) = 1 then
                  PC := PC + I.Off;
               else
                  PC := PC + 1;
               end if;
         end case;
      end;
   end loop;

   Ada.Long_Long_Integer_Text_IO.Put (Item => Registers (B), Width => 1);
   Ada.Text_IO.New_Line;

end Solve;
