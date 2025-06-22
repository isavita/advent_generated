
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;

procedure Maze_Solver is

   package Integer_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Integer);

   Offsets    : Integer_Vectors.Vector;
   Input_File : Ada.Text_IO.File_Type;
   Value      : Integer;
   Index      : Integer := 1;
   Steps      : Natural := 0;
   Jump       : Integer;

begin
   Ada.Text_IO.Open(File => Input_File, Mode => Ada.Text_IO.In_File, Name => "input.txt");

   while not Ada.Text_IO.End_Of_File(Input_File) loop
      Ada.Integer_Text_IO.Get(File => Input_File, Item => Value);
      Offsets.Append(Value);
   end loop;

   Ada.Text_IO.Close(Input_File);

   while Index >= Offsets.First_Index and Index <= Offsets.Last_Index loop
      Jump := Offsets.Element(Index);
      Offsets.Replace_Element(Index, Jump + 1);
      Index := Index + Jump;
      Steps := Steps + 1;
   end loop;

   Ada.Integer_Text_IO.Put(Steps);
   Ada.Text_IO.New_Line;

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line("File 'input.txt' not found.");
   when others =>
      Ada.Text_IO.Put_Line("An error occurred.");

end Maze_Solver;
