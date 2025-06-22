
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Main is

   function Read_Input (Filename : String) return Integer is
      File_Handle : Ada.Text_IO.File_Type;
      Value       : Integer;
   begin
      Ada.Text_IO.Open (File_Handle, Ada.Text_IO.In_File, Filename);
      Ada.Integer_Text_IO.Get (File_Handle, Value);
      Ada.Text_IO.Close (File_Handle);
      return Value;
   exception
      when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error | Ada.Text_IO.Data_Error =>
         raise Program_Error;
      when others =>
         raise Program_Error;
   end Read_Input;

   function Find_Winning_Elf (Total_Elves : Integer) return Integer is
      Highest_Power_Of_Two : Integer := 1;
   begin
      while Highest_Power_Of_Two * 2 <= Total_Elves loop
         Highest_Power_Of_Two := Highest_Power_Of_Two * 2;
      end loop;
      return (Total_Elves - Highest_Power_Of_Two) * 2 + 1;
   end Find_Winning_Elf;

   Total_Elves : Integer;
   Winner      : Integer;

begin
   Total_Elves := Read_Input ("input.txt");
   Winner      := Find_Winning_Elf (Total_Elves);
   Ada.Integer_Text_IO.Put (Winner);
   Ada.Text_IO.New_Line;
end Main;
