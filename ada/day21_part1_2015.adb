
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   type Item is record
      Cost   : Natural := 0;
      Damage : Natural := 0;
      Armor  : Natural := 0;
   end record;

   type Item_Array is array (Positive range <>) of Item;

   Weapons : constant Item_Array :=
     ((Cost => 8,   Damage => 4, Armor => 0),
      (Cost => 10,  Damage => 5, Armor => 0),
      (Cost => 25,  Damage => 6, Armor => 0),
      (Cost => 40,  Damage => 7, Armor => 0),
      (Cost => 74,  Damage => 8, Armor => 0));

   Armor_Items : constant Item_Array :=
     ((Cost => 0,   Damage => 0, Armor => 0),
      (Cost => 13,  Damage => 0, Armor => 1),
      (Cost => 31,  Damage => 0, Armor => 2),
      (Cost => 53,  Damage => 0, Armor => 3),
      (Cost => 75,  Damage => 0, Armor => 4),
      (Cost => 102, Damage => 0, Armor => 5));

   Ring_Items : constant Item_Array :=
     ((Cost => 0,   Damage => 0, Armor => 0),
      (Cost => 25,  Damage => 1, Armor => 0),
      (Cost => 50,  Damage => 2, Armor => 0),
      (Cost => 100, Damage => 3, Armor => 0),
      (Cost => 20,  Damage => 0, Armor => 1),
      (Cost => 40,  Damage => 0, Armor => 2),
      (Cost => 80,  Damage => 0, Armor => 3));

   Player_HP   : constant Positive := 100;
   Boss_HP     : Positive;
   Boss_Damage : Natural;
   Boss_Armor  : Natural;

   Input_File : Ada.Text_IO.File_Type;
   Min_Cost   : Natural := Natural'Last;

   procedure Read_Stat (Value : out Natural) is
      Line      : constant String  := Ada.Text_IO.Get_Line (Input_File);
      Separator : constant Natural := Ada.Strings.Fixed.Index (Line, ":");
      Value_Str : constant String  := Line (Separator + 2 .. Line'Last);
      Last      : Natural;
   begin
      Ada.Integer_Text_IO.Get (From => Value_Str, Item => Value, Last => Last);
   end Read_Stat;

begin
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   Read_Stat (Boss_HP);
   Read_Stat (Boss_Damage);
   Read_Stat (Boss_Armor);
   Ada.Text_IO.Close (Input_File);

   for W of Weapons loop
      for A of Armor_Items loop
         for I in Ring_Items'Range loop
            for J in I + 1 .. Ring_Items'Last loop
               declare
                  R1 : constant Item := Ring_Items (I);
                  R2 : constant Item := Ring_Items (J);

                  Current_Cost    : constant Natural := W.Cost + A.Cost + R1.Cost + R2.Cost;
                  Player_Dmg_Stat : constant Natural := W.Damage + A.Damage + R1.Damage + R2.Damage;
                  Player_Arm_Stat : constant Natural := W.Armor + A.Armor + R1.Armor + R2.Armor;

                  Player_Net_Dmg  : constant Positive := Natural'Max (1, Player_Dmg_Stat - Boss_Armor);
                  Boss_Net_Dmg    : constant Positive := Natural'Max (1, Boss_Damage - Player_Arm_Stat);

                  Player_Turns    : constant Positive := (Boss_HP + Player_Net_Dmg - 1) / Player_Net_Dmg;
                  Boss_Turns      : constant Positive := (Player_HP + Boss_Net_Dmg - 1) / Boss_Net_Dmg;
               begin
                  if Current_Cost < Min_Cost and then Player_Turns <= Boss_Turns then
                     Min_Cost := Current_Cost;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;

   Ada.Integer_Text_IO.Put (Item => Min_Cost, Width => 1);
   Ada.Text_IO.New_Line;

end Main;
