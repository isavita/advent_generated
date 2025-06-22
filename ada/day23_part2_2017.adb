
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions;

procedure Solve is

   function Is_Prime (N : in Integer) return Boolean is
      Limit : Integer;
      I     : Integer;
   begin
      if N <= 1 then
         return False;
      end if;
      if N = 2 then
         return True;
      end if;
      if N mod 2 = 0 then
         return False;
      end if;

      Limit := Integer(Ada.Numerics.Elementary_Functions.Sqrt(Float(N)));

      I := 3;
      while I <= Limit loop
         if N mod I = 0 then
            return False;
         end if;
         I := I + 2;
      end loop;

      return True;
   end Is_Prime;

   B : constant Integer := 57 * 100 + 100000;
   C : constant Integer := B + 17000;
   H : Integer := 0;
   X : Integer;

begin
   X := B;
   while X <= C loop
      if not Is_Prime(X) then
         H := H + 1;
      end if;
      X := X + 17;
   end loop;

   Ada.Integer_Text_IO.Put(H);
   Ada.Text_IO.New_Line;

end Solve;
