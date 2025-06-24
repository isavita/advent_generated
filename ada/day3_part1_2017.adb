
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions;

procedure Main is
   function Find_Steps (Num : in Integer) return Integer is
      use Ada.Numerics.Elementary_Functions;
      Root : Integer;
   begin
      if Num = 1 then
         return 0;
      end if;

      Root := Integer (Float'Ceiling (Sqrt (Float (Num))));

      if Root mod 2 = 0 then
         Root := Root + 1;
      end if;

      declare
         Side_Length     : constant Integer := Root - 1;
         Steps_To_Center : constant Integer := Side_Length / 2;
      begin
         return Steps_To_Center +
           abs (((Root ** 2) - Num) mod Side_Length - Steps_To_Center);
      end;
   end Find_Steps;

   Input_File : Ada.Text_IO.File_Type;
   Num        : Integer;
begin
   Ada.Text_IO.Open
     (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   Ada.Integer_Text_IO.Get (File => Input_File, Item => Num);
   Ada.Text_IO.Close (File => Input_File);

   Ada.Integer_Text_IO.Put (Item => Find_Steps (Num), Width => 1);
   Ada.Text_IO.New_Line;
end Main;
