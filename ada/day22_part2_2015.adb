
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   type Game_State is record
      Player_HP      : Integer := 50;
      Player_Mana    : Integer := 500;
      Boss_HP        : Integer := 0;
      Boss_Damage    : Integer := 0;
      Shield_Timer   : Integer := 0;
      Poison_Timer   : Integer := 0;
      Recharge_Timer : Integer := 0;
      Mana_Spent     : Integer := 0;
   end record;

   function Min_Mana_To_Win (Initial_State : Game_State; Hard_Mode : Boolean) return Integer is
      Min_Mana : Integer := Integer'Last;

      procedure Simulate (State : in Game_State; Player_Turn : in Boolean) is
         Current_State : Game_State := State;
      begin
         if Current_State.Mana_Spent >= Min_Mana then
            return;
         end if;

         if Player_Turn and Hard_Mode then
            Current_State.Player_HP := Current_State.Player_HP - 1;
            if Current_State.Player_HP <= 0 then
               return;
            end if;
         end if;

         if Current_State.Shield_Timer > 0 then
            Current_State.Shield_Timer := Current_State.Shield_Timer - 1;
         end if;
         if Current_State.Poison_Timer > 0 then
            Current_State.Boss_HP := Current_State.Boss_HP - 3;
            Current_State.Poison_Timer := Current_State.Poison_Timer - 1;
         end if;
         if Current_State.Recharge_Timer > 0 then
            Current_State.Player_Mana := Current_State.Player_Mana + 101;
            Current_State.Recharge_Timer := Current_State.Recharge_Timer - 1;
         end if;

         if Current_State.Boss_HP <= 0 then
            Min_Mana := Integer'Min (Min_Mana, Current_State.Mana_Spent);
            return;
         end if;

         if not Player_Turn then
            declare
               Damage : Integer := Current_State.Boss_Damage;
            begin
               if Current_State.Shield_Timer > 0 then
                  Damage := Damage - 7;
               end if;
               Current_State.Player_HP := Current_State.Player_HP - Integer'Max (1, Damage);
               if Current_State.Player_HP > 0 then
                  Simulate (Current_State, True);
               end if;
            end;
            return;
         end if;

         if Current_State.Player_Mana >= 53 then
            declare
               New_State : Game_State := Current_State;
            begin
               New_State.Player_Mana := New_State.Player_Mana - 53;
               New_State.Mana_Spent  := New_State.Mana_Spent + 53;
               New_State.Boss_HP     := New_State.Boss_HP - 4;
               Simulate (New_State, False);
            end;
         end if;
         if Current_State.Player_Mana >= 73 then
            declare
               New_State : Game_State := Current_State;
            begin
               New_State.Player_Mana := New_State.Player_Mana - 73;
               New_State.Mana_Spent  := New_State.Mana_Spent + 73;
               New_State.Boss_HP     := New_State.Boss_HP - 2;
               New_State.Player_HP   := New_State.Player_HP + 2;
               Simulate (New_State, False);
            end;
         end if;
         if Current_State.Player_Mana >= 113 and Current_State.Shield_Timer = 0 then
            declare
               New_State : Game_State := Current_State;
            begin
               New_State.Player_Mana  := New_State.Player_Mana - 113;
               New_State.Mana_Spent   := New_State.Mana_Spent + 113;
               New_State.Shield_Timer := 6;
               Simulate (New_State, False);
            end;
         end if;
         if Current_State.Player_Mana >= 173 and Current_State.Poison_Timer = 0 then
            declare
               New_State : Game_State := Current_State;
            begin
               New_State.Player_Mana  := New_State.Player_Mana - 173;
               New_State.Mana_Spent   := New_State.Mana_Spent + 173;
               New_State.Poison_Timer := 6;
               Simulate (New_State, False);
            end;
         end if;
         if Current_State.Player_Mana >= 229 and Current_State.Recharge_Timer = 0 then
            declare
               New_State : Game_State := Current_State;
            begin
               New_State.Player_Mana    := New_State.Player_Mana - 229;
               New_State.Mana_Spent     := New_State.Mana_Spent + 229;
               New_State.Recharge_Timer := 5;
               Simulate (New_State, False);
            end;
         end if;
      end Simulate;

   begin
      Simulate (Initial_State, True);
      return Min_Mana;
   end Min_Mana_To_Win;

   Input_File    : Ada.Text_IO.File_Type;
   Initial_State : Game_State;
begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   declare
      Line      : String    := Ada.Text_IO.Get_Line (Input_File);
      Colon_Pos : Natural   := Ada.Strings.Fixed.Index (Line, ": ");
      Value_Str : String    := Line (Colon_Pos + 2 .. Line'Last);
   begin
      Initial_State.Boss_HP := Integer'Value (Value_Str);
   end;
   declare
      Line      : String    := Ada.Text_IO.Get_Line (Input_File);
      Colon_Pos : Natural   := Ada.Strings.Fixed.Index (Line, ": ");
      Value_Str : String    := Line (Colon_Pos + 2 .. Line'Last);
   begin
      Initial_State.Boss_Damage := Integer'Value (Value_Str);
   end;
   Ada.Text_IO.Close (Input_File);

   Ada.Integer_Text_IO.Put (Item => Min_Mana_To_Win (Initial_State, True), Width => 1);
   Ada.Text_IO.New_Line;
end Main;
