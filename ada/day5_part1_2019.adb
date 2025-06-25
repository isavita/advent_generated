
-- main.adb
--
-- Solves the Advent of Code "Day 5: Sunny with a Chance of Asteroids" challenge.
-- This program implements an Intcode computer with support for new opcodes (3 and 4)
-- and parameter modes (position and immediate).
--
-- It reads an Intcode program from "input.txt", executes it with a fixed
-- input of 1, and prints all resulting outputs to the console. The final
-- output before the program halts is the diagnostic code.
--

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.IO_Exceptions;
with Ada.Exceptions;

procedure Main is

   -- Define the memory type using a vector of Integers.
   -- The index starts from 0, which is convenient for this problem's addressing.
   package Intcode_Memory_Pkg is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);
   subtype Memory_Type is Intcode_Memory_Pkg.Vector;

   -- Program memory and file handling variables
   Memory     : Memory_Type;
   Input_File : Ada.Text_IO.File_Type;
   File_Name  : constant String := "input.txt";

begin
   -- Part 1: Read and parse the Intcode program from the input file.
   -- This block handles file opening and parsing, ensuring the file is closed
   -- and handling potential errors like a missing file.
   begin
      Ada.Text_IO.Open
        (File => Input_File, Mode => Ada.Text_IO.In_File, Name => File_Name);

      declare
         Line            : constant String := Ada.Text_IO.Get_Line (Input_File);
         Start_Of_Number : Positive := Line'First;
      begin
         -- Iterate through the line to find commas, which delimit the numbers.
         for I in Line'Range loop
            if Line (I) = ',' then
               Memory.Append (Integer'Value (Line (Start_Of_Number .. I - 1)));
               Start_Of_Number := I + 1;
            end if;
         end loop;
         -- Append the final number after the last comma.
         Memory.Append (Integer'Value (Line (Start_Of_Number .. Line'Last)));
      end;

      Ada.Text_IO.Close (Input_File);

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line ("Error: Input file '" & File_Name & "' not found.");
         return;
   end;

   -- Part 2: Execute the Intcode program.
   -- This block contains the main execution loop and the Intcode logic.
   declare
      IP : Natural := 0; -- Instruction Pointer

      -- Helper function to get a parameter's value based on its mode.
      -- Mode 0 (Position): The parameter is a pointer to the value.
      -- Mode 1 (Immediate): The parameter is the value itself.
      -- This function is nested to show it's only used within this execution context.
      function Get_Value
        (Mem    : in Memory_Type;
         Ptr    : in Natural;
         Offset : in Natural;
         Mode   : in Integer) return Integer
      is
         Parameter_Location : constant Natural := Ptr + Offset;
      begin
         if Mode = 1 then  -- Immediate Mode
            return Mem (Parameter_Location);
         else  -- Position Mode (Mode = 0)
            return Mem (Mem (Parameter_Location));
         end if;
      end Get_Value;

   begin
      -- Main execution loop, continues until a halt (opcode 99) is encountered.
      loop
         -- Decode the instruction at the current instruction pointer (IP).
         -- The opcode is the last two digits. Parameter modes are the digits
         -- to the left of the opcode.
         declare
            Instruction : constant Integer := Memory (IP);
            Opcode      : constant Integer := Instruction mod 100;
            Mode1       : constant Integer := (Instruction / 100) mod 10;
            Mode2       : constant Integer := (Instruction / 1000) mod 10;
         begin
            case Opcode is

               when 1 =>  -- Add: Opcode, Param1, Param2, Dest
                  declare
                     Val1      : constant Integer := Get_Value (Memory, IP, 1, Mode1);
                     Val2      : constant Integer := Get_Value (Memory, IP, 2, Mode2);
                     Dest_Addr : constant Natural := Memory (IP + 3);
                  begin
                     Memory.Replace_Element (Dest_Addr, Val1 + Val2);
                     IP := IP + 4;
                  end;

               when 2 =>  -- Multiply: Opcode, Param1, Param2, Dest
                  declare
                     Val1      : constant Integer := Get_Value (Memory, IP, 1, Mode1);
                     Val2      : constant Integer := Get_Value (Memory, IP, 2, Mode2);
                     Dest_Addr : constant Natural := Memory (IP + 3);
                  begin
                     Memory.Replace_Element (Dest_Addr, Val1 * Val2);
                     IP := IP + 4;
                  end;

               when 3 =>  -- Input: Opcode, Dest
                  declare
                     Dest_Addr   : constant Natural := Memory (IP + 1);
                     -- Per problem specification, provide 1 as input for the AC unit.
                     Input_Value : constant Integer := 1;
                  begin
                     Memory.Replace_Element (Dest_Addr, Input_Value);
                     IP := IP + 2;
                  end;

               when 4 =>  -- Output: Opcode, Param1
                  declare
                     Output_Value : constant Integer := Get_Value (Memory, IP, 1, Mode1);
                  begin
                     Ada.Integer_Text_IO.Put (Item => Output_Value, Width => 1);
                     Ada.Text_IO.New_Line;
                     IP := IP + 2;
                  end;

               when 99 => -- Halt
                  exit; -- Exit the execution loop

               when others =>
                  Ada.Text_IO.Put_Line ("Error: Unknown opcode " & Integer'Image (Opcode) & " at position " & Natural'Image (IP));
                  return;
            end case;
         end;
      end loop;
   end;

exception
   -- Catch-all for any other runtime errors to provide a clean exit.
   when E : others =>
      Ada.Text_IO.Put_Line ("An unexpected error occurred:");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end Main;
