
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;

procedure Main is
   type Item is record
      Cost   : Natural := 0;
      Damage : Natural := 0;
      Armor  : Natural := 0;
   end record;

   type Character is record
      Hit_Points : Positive;
      Damage     : Natural;
      Armor      : Natural;
   end record;

   type Item_Array is array (Positive range <>) of Item;

   Weapons : constant Item_Array :=
     ((Cost => 8,  Damage => 4, Armor => 0),
      (Cost => 10, Damage => 5, Armor => 0),
      (Cost => 25, Damage => 6, Armor => 0),
      (Cost => 40, Damage => 7, Armor => 0),
      (Cost => 74, Damage => 8, Armor => 0));

   Armors : constant Item_Array :=
     ((Cost => 0,   Damage => 0, Armor => 0),
      (Cost => 13,  Damage => 0, Armor => 1),
      (Cost => 31,  Damage => 0, Armor => 2),
      (Cost => 53,  Damage => 0, Armor => 3),
      (Cost => 75,  Damage => 0, Armor => 4),
      (Cost => 102, Damage => 0, Armor => 5));

   Rings : constant Item_Array :=
     ((Cost => 0,   Damage => 0, Armor => 0),
      (Cost => 25,  Damage => 1, Armor => 0),
      (Cost => 50,  Damage => 2, Armor => 0),
      (Cost => 100, Damage => 3, Armor => 0),
      (Cost => 20,  Damage => 0, Armor => 1),
      (Cost => 40,  Damage => 0, Armor => 2),
      (Cost => 80,  Damage => 0, Armor => 3));

   function Parse_Stat (Line : String) return Natural is
      Colon_Pos : constant Natural := Ada.Strings.Fixed.Index (Line, ": ");
   begin
      return Natural'Value (Line (Colon_Pos + 2 .. Line'Last));
   end Parse_Stat;

   function Player_Wins (Player, Boss : Character) return Boolean is
      Player_Damage : constant Positive :=
        Positive'Max (1, Integer (Player.Damage) - Integer (Boss.Armor));
      Boss_Damage   : constant Positive :=
        Positive'Max (1, Integer (Boss.Damage) - Integer (Player.Armor));
      Player_Turns  : constant Positive :=
        (Boss.Hit_Points + Player_Damage - 1) / Player_Damage;
      Boss_Turns    : constant Positive :=
        (Player.Hit_Points + Boss_Damage - 1) / Boss_Damage;
   begin
      return Player_Turns <= Boss_Turns;
   end Player_Wins;

   Input    : Ada.Text_IO.File_Type;
   Boss     : Character := (Hit_Points => 1, Damage => 0, Armor => 0);
   Max_Cost : Natural   := 0;

begin
   Ada.Text_IO.Open (File => Input, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   Boss.Hit_Points := Positive (Parse_Stat (Ada.Text_IO.Get_Line (Input)));
   Boss.Damage     := Parse_Stat (Ada.Text_IO.Get_Line (Input));
   Boss.Armor      := Parse_Stat (Ada.Text_IO.Get_Line (Input));
   Ada.Text_IO.Close (Input);

   for W of Weapons loop
      for A of Armors loop
         for I in Rings'Range loop
            for J in I + 1 .. Rings'Last loop
               declare
                  R1     : constant Item := Rings (I);
                  R2     : constant Item := Rings (J);
                  Player : Character :=
                    (Hit_Points => 100,
                     Damage     => W.Damage + R1.Damage + R2.Damage,
                     Armor      => A.Armor + R1.Armor + R2.Armor);
                  Cost   : constant Natural := W.Cost + A.Cost + R1.Cost + R2.Cost;
               begin
                  if not Player_Wins (Player, Boss) and then Cost > Max_Cost then
                     Max_Cost := Cost;
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;

   Ada.Integer_Text_IO.Put (Item => Max_Cost, Width => 1);
   Ada.Text_IO.New_Line;

end Main;
