
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Solve is

   type Game_State is record
      Player_HP, Player_Mana, Boss_HP, Boss_Damage : Integer;
      Shield_Timer, Poison_Timer, Recharge_Timer   : Integer;
      Mana_Spent                                   : Integer;
   end record;

   Min_Mana_Global : Integer := Integer'Last;

   procedure Simulate (Current_State : in Game_State; Player_Turn : in Boolean) is
      State_After_Effects : Game_State := Current_State;
      Next_State          : Game_State;
   begin
      if State_After_Effects.Mana_Spent >= Min_Mana_Global then
         return;
      end if;

      if State_After_Effects.Shield_Timer > 0 then
         State_After_Effects.Shield_Timer := State_After_Effects.Shield_Timer - 1;
      end if;
      if State_After_Effects.Poison_Timer > 0 then
         State_After_Effects.Boss_HP := State_After_Effects.Boss_HP - 3;
         State_After_Effects.Poison_Timer := State_After_Effects.Poison_Timer - 1;
      end if;
      if State_After_Effects.Recharge_Timer > 0 then
         State_After_Effects.Player_Mana := State_After_Effects.Player_Mana + 101;
         State_After_Effects.Recharge_Timer := State_After_Effects.Recharge_Timer - 1;
      end if;

      if State_After_Effects.Boss_HP <= 0 then
         Min_Mana_Global := Integer'Min(Min_Mana_Global, State_After_Effects.Mana_Spent);
         return;
      end if;
      if State_After_Effects.Player_HP <= 0 then
         return;
      end if;

      if not Player_Turn then
         declare
            Damage : Integer := State_After_Effects.Boss_Damage;
         begin
            if State_After_Effects.Shield_Timer > 0 then
               Damage := Damage - 7;
            end if;
            if Damage < 1 then
               Damage := 1;
            end if;
            State_After_Effects.Player_HP := State_After_Effects.Player_HP - Damage;
            Simulate(State_After_Effects, True);
         end;
         return;
      end if;

      if State_After_Effects.Player_Mana >= 53 then
         Next_State := State_After_Effects;
         Next_State.Player_Mana := Next_State.Player_Mana - 53;
         Next_State.Mana_Spent := Next_State.Mana_Spent + 53;
         Next_State.Boss_HP := Next_State.Boss_HP - 4;
         Simulate(Next_State, False);
      end if;

      if State_After_Effects.Player_Mana >= 73 then
         Next_State := State_After_Effects;
         Next_State.Player_Mana := Next_State.Player_Mana - 73;
         Next_State.Mana_Spent := Next_State.Mana_Spent + 73;
         Next_State.Boss_HP := Next_State.Boss_HP - 2;
         Next_State.Player_HP := Next_State.Player_HP + 2;
         Simulate(Next_State, False);
      end if;

      if State_After_Effects.Player_Mana >= 113 and State_After_Effects.Shield_Timer = 0 then
         Next_State := State_After_Effects;
         Next_State.Player_Mana := Next_State.Player_Mana - 113;
         Next_State.Mana_Spent := Next_State.Mana_Spent + 113;
         Next_State.Shield_Timer := 6;
         Simulate(Next_State, False);
      end if;

      if State_After_Effects.Player_Mana >= 173 and State_After_Effects.Poison_Timer = 0 then
         Next_State := State_After_Effects;
         Next_State.Player_Mana := Next_State.Player_Mana - 173;
         Next_State.Mana_Spent := Next_State.Mana_Spent + 173;
         Next_State.Poison_Timer := 6;
         Simulate(Next_State, False);
      end if;

      if State_After_Effects.Player_Mana >= 229 and State_After_Effects.Recharge_Timer = 0 then
         Next_State := State_After_Effects;
         Next_State.Player_Mana := Next_State.Player_Mana - 229;
         Next_State.Mana_Spent := Next_State.Mana_Spent + 229;
         Next_State.Recharge_Timer := 5;
         Simulate(Next_State, False);
      end if;
   end Simulate;

   function Parse_Value (S : String) return Integer is
      Colon_Pos : Natural := Ada.Strings.Fixed.Index(S, ": ");
      Value_Str : String := S(Colon_Pos + 2 .. S'Last);
      Value     : Integer;
      Last_Char : Natural;
   begin
      Ada.Integer_Text_IO.Get(Value_Str, Value, Last_Char);
      return Value;
   end Parse_Value;

   File_Name          : constant String := "input.txt";
   Input_File         : Ada.Text_IO.File_Type;
   Line_Buffer        : String(1..100);
   Line_Length        : Natural;
   Boss_HP_Val        : Integer;
   Boss_Damage_Val    : Integer;
   Initial_Game_State : Game_State;

begin
   Ada.Text_IO.Open(Input_File, Ada.Text_IO.In_File, File_Name);

   Ada.Text_IO.Get_Line(Input_File, Line_Buffer, Line_Length);
   Boss_HP_Val := Parse_Value(Line_Buffer(1..Line_Length));

   Ada.Text_IO.Get_Line(Input_File, Line_Buffer, Line_Length);
   Boss_Damage_Val := Parse_Value(Line_Buffer(1..Line_Length));

   Ada.Text_IO.Close(Input_File);

   Initial_Game_State := (
      Player_HP      => 50,
      Player_Mana    => 500,
      Boss_HP        => Boss_HP_Val,
      Boss_Damage    => Boss_Damage_Val,
      Shield_Timer   => 0,
      Poison_Timer   => 0,
      Recharge_Timer => 0,
      Mana_Spent     => 0
   );

   Simulate(Initial_Game_State, True);

   Ada.Text_IO.Put_Line(Integer'Image(Min_Mana_Global));

end Solve;
