
with Ada.Text_IO;

procedure Solve is

   function Contains_ABBA (S : in String) return Boolean is
   begin
      if S'Length < 4 then
         return False;
      end if;
      for I in S'First .. S'Last - 3 loop
         if S(I) /= S(I+1) and then
            S(I)  = S(I+3) and then
            S(I+1) = S(I+2)
         then
            return True;
         end if;
      end loop;
      return False;
   end Contains_ABBA;

   function Supports_TLS (IP : in String) return Boolean is
      Found_ABBA_In_Supernet : Boolean := False;
      Found_ABBA_In_Hypernet : Boolean := False;

      Current_Pos : Natural := IP'First;
      Start_Segment : Natural := IP'First;
      In_Bracket : Boolean := False;

      Bracket_Open_Char  : constant Character := '[';
      Bracket_Close_Char : constant Character := ']';

   begin
      if IP'Length = 0 then
         return False;
      end if;

      while Current_Pos <= IP'Last loop
         if IP(Current_Pos) = Bracket_Open_Char then
            if Current_Pos > Start_Segment then
               declare
                  Supernet_Segment : constant String := IP(Start_Segment .. Current_Pos - 1);
               begin
                  if Contains_ABBA(Supernet_Segment) then
                     Found_ABBA_In_Supernet := True;
                  end if;
               end;
            end if;
            In_Bracket := True;
            Start_Segment := Current_Pos + 1;
         elsif IP(Current_Pos) = Bracket_Close_Char then
            if Current_Pos > Start_Segment then
               declare
                  Hypernet_Segment : constant String := IP(Start_Segment .. Current_Pos - 1);
               begin
                  if Contains_ABBA(Hypernet_Segment) then
                     Found_ABBA_In_Hypernet := True;
                     return False;
                  end if;
               end;
            end if;
            In_Bracket := False;
            Start_Segment := Current_Pos + 1;
         end if;
         Current_Pos := Current_Pos + 1;
      end loop;

      if Current_Pos > Start_Segment then
         declare
            Last_Segment : constant String := IP(Start_Segment .. Current_Pos - 1);
         begin
            if not In_Bracket then
               if Contains_ABBA(Last_Segment) then
                  Found_ABBA_In_Supernet := True;
               end if;
            end if;
         end;
      end if;

      return Found_ABBA_In_Supernet and not Found_ABBA_In_Hypernet;
   end Supports_TLS;

   Input_File : Ada.Text_IO.File_Type;
   Line       : String (1 .. 256);
   Last       : Natural;
   TLS_Count  : Natural := 0;

begin
   Ada.Text_IO.Open (File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => "input.txt");

   while not Ada.Text_IO.End_Of_File (Input_File) loop
      Ada.Text_IO.Get_Line (File => Input_File, Item => Line, Last => Last);
      if Supports_TLS (Line (1 .. Last)) then
         TLS_Count := TLS_Count + 1;
      end if;
   end loop;

   Ada.Text_IO.Close (Input_File);
   Ada.Text_IO.Put_Line (Natural'Image (TLS_Count));

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Error: input.txt not found.");
   when others =>
      Ada.Text_IO.Put_Line ("An unexpected error occurred.");
end Solve;
