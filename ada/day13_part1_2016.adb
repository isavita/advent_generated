
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded;

procedure Main is
   type Point is record
      X, Y : Natural;
   end record;

   function Hash (P : Point) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type'Mod (P.X * 5381 + P.Y);
   end Hash;

   type State is record
      P     : Point;
      Steps : Natural;
   end record;

   package State_Queues is new Ada.Containers.Doubly_Linked_Lists (State);
   package Point_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Point,
      Hash                => Hash,
      Equivalent_Elements => "=");

   Favorite_Number : Natural;

   function Is_Wall (X, Y : Natural) return Boolean is
      Num   : constant Natural := X*X + 3*X + 2*X*Y + Y + Y*Y + Favorite_Number;
      Count : Natural          := 0;
      Temp  : Natural          := Num;
   begin
      while Temp > 0 loop
         if Temp mod 2 = 1 then
            Count := Count + 1;
         end if;
         Temp := Temp / 2;
      end loop;
      return Count mod 2 /= 0;
   end Is_Wall;

   function BFS (Start_Pos, Target_Pos : Point) return Natural is
      Queue   : State_Queues.List;
      Visited : Point_Sets.Set;
      Deltas  : constant array (1 .. 4, 1 .. 2) of Integer :=
        ((1, 0), (-1, 0), (0, 1), (0, -1));
   begin
      Queue.Append ((P => Start_Pos, Steps => 0));

      while not Queue.Is_Empty loop
         declare
            Current_State : constant State   := Queue.First_Element;
            Current_Pos   : constant Point   := Current_State.P;
            Current_Steps : constant Natural := Current_State.Steps;
         begin
            Queue.Delete_First;

            if Current_Pos = Target_Pos then
               return Current_Steps;
            end if;

            if Visited.Contains (Current_Pos) or else Is_Wall (Current_Pos.X, Current_Pos.Y) then
               null;
            else
               Visited.Insert (Current_Pos);
               for I in Deltas'Range (1) loop
                  declare
                     New_X : constant Integer := Integer (Current_Pos.X) + Deltas (I, 1);
                     New_Y : constant Integer := Integer (Current_Pos.Y) + Deltas (I, 2);
                  begin
                     if New_X >= 0 and New_Y >= 0 then
                        Queue.Append
                          ((P     => (X => Natural (New_X), Y => Natural (New_Y)),
                            Steps => Current_Steps + 1));
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;
      return 0;
   end BFS;

   File         : Ada.Text_IO.File_Type;
   Start_Point  : constant Point := (X => 1, Y => 1);
   Target_Point : constant Point := (X => 31, Y => 39);
begin
   Ada.Text_IO.Open (File, Mode => Ada.Text_IO.In_File, Name => "input.txt");
   declare
      Line : constant String := Ada.Text_IO.Get_Line (File);
   begin
      Favorite_Number := Natural'Value (Line);
   end;
   Ada.Text_IO.Close (File);

   Ada.Integer_Text_IO.Put (Item => BFS (Start_Point, Target_Point), Width => 1);
   Ada.Text_IO.New_Line;
end Main;
