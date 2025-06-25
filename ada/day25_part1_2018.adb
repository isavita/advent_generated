
-- This program solves the Day 25 challenge by finding the number of
-- constellations in a set of 4D spacetime points. It reads coordinates
-- from "input.txt" and prints the result to standard output.
--
-- The solution models the problem as finding connected components in a graph,
-- where points are nodes and an edge exists between points if their
-- Manhattan distance is 3 or less. The Disjoint Set Union (DSU) algorithm
-- with path compression is used for an efficient solution.

-- Standard library packages
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

-- Ada containers for a dynamic array of points
with Ada.Containers.Vectors;

-- The main procedure serves as the program's entry point.
procedure Main is

   -- Define a record to represent a 4D point in spacetime.
   type Point_Type is record
      W, X, Y, Z : Integer;
   end record;

   -- Instantiate the generic Vectors package for our Point_Type.
   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Point_Type);

   -- Calculates the Manhattan distance between two 4D points.
   function Manhattan_Distance (P1, P2 : Point_Type) return Natural is
   begin
      return Natural (abs (P1.W - P2.W) +
                      abs (P1.X - P2.X) +
                      abs (P1.Y - P2.Y) +
                      abs (P1.Z - P2.Z));
   end Manhattan_Distance;

   -- Vector to store all points read from the input file.
   Points : Point_Vectors.Vector;

   -- Input file handling variables.
   Input_File : Ada.Text_IO.File_Type;
   File_Name  : String := "input.txt";

begin
   -- Allow specifying the input file via a command-line argument.
   if Ada.Command_Line.Argument_Count >= 1 then
      File_Name := Ada.Command_Line.Argument (1);
   end if;

   -- Open the specified input file for reading.
   Ada.Text_IO.Open (File => Input_File, Mode => Ada.Text_IO.In_File, Name => File_Name);

   -- Read all points from the file until it ends.
   loop
      -- This local block handles parsing for a single point.
      declare
         W, X, Y, Z : Integer;
         Comma      : Character;
      begin
         -- Read four integers separated by commas.
         Ada.Integer_Text_IO.Get (File => Input_File, Item => W);
         Ada.Text_IO.Get (File => Input_File, Item => Comma);
         if Comma /= ',' then raise Ada.Text_IO.Data_Error; end if;

         Ada.Integer_Text_IO.Get (File => Input_File, Item => X);
         Ada.Text_IO.Get (File => Input_File, Item => Comma);
         if Comma /= ',' then raise Ada.Text_IO.Data_Error; end if;

         Ada.Integer_Text_IO.Get (File => Input_File, Item => Y);
         Ada.Text_IO.Get (File => Input_File, Item => Comma);
         if Comma /= ',' then raise Ada.Text_IO.Data_Error; end if;

         Ada.Integer_Text_IO.Get (File => Input_File, Item => Z);

         Points.Append ((W, X, Y, Z));

         -- Ensure we move to the next line, skipping any trailing characters.
         if not Ada.Text_IO.End_Of_Line (Input_File) then
            Ada.Text_IO.Skip_Line (Input_File);
         end if;

      exception
         -- End_Error is the standard way to detect the end of the file when using Get.
         when Ada.Text_IO.End_Error =>
            exit;
         -- Skip malformed or empty lines and continue.
         when Ada.Text_IO.Data_Error =>
            if not Ada.Text_IO.End_Of_File (Input_File) then
               Ada.Text_IO.Skip_Line (Input_File);
            else
               exit;
            end if;
      end;
   end loop;

   Ada.Text_IO.Close (Input_File);

   -- If the input file was empty, there are no constellations.
   if Points.Is_Empty then
      Ada.Text_IO.Put_Line ("0");
      return;
   end if;

   -- This block encapsulates the Disjoint Set Union (DSU) logic.
   declare
      Num_Points : constant Natural := Natural (Points.Length);

      -- The Parent array is the core of the DSU data structure.
      -- Parent(I) stores the parent of element I. A root has itself as parent.
      Parent : array (1 .. Num_Points) of Positive;

      -- Finds the representative (root) of the set containing element I.
      -- It performs path compression for optimization by making every node
      -- on the find path point directly to the root.
      function Find (I : Positive) return Positive is
      begin
         if Parent (I) = I then
            return I;
         else
            Parent (I) := Find (Parent (I)); -- Recursive call with path compression
            return Parent (I);
         end if;
      end Find;

      -- Merges the sets containing elements I and J.
      procedure Union (I, J : Positive) is
         Root_I : Positive := Find (I);
         Root_J : Positive := Find (J);
      begin
         if Root_I /= Root_J then
            Parent (Root_I) := Root_J;
         end if;
      end Union;

   begin
      -- Initialize the DSU: each point starts in its own set (constellation).
      for I in Parent'Range loop
         Parent (I) := I;
      end loop;

      -- Iterate through all unique pairs of points. If their distance is <= 3,
      -- they belong to the same constellation, so we merge their sets.
      for I in 1 .. Num_Points loop
         for J in I + 1 .. Num_Points loop
            if Manhattan_Distance (Points.Element (I), Points.Element (J)) <= 3 then
               Union (I, J);
            end if;
         end loop;
      end loop;

      -- Count the number of constellations by counting the number of disjoint sets.
      -- This is equal to the number of root nodes in the DSU forest.
      declare
         Constellation_Count : Natural := 0;
      begin
         for I in Parent'Range loop
            if Parent (I) = I then -- A node is a root if it's its own parent.
               Constellation_Count := Constellation_Count + 1;
            end if;
         end loop;

         -- Print the final result to standard output.
         Ada.Integer_Text_IO.Put (Item => Constellation_Count, Width => 1);
         Ada.Text_IO.New_Line;
      end;
   end;

exception
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Error: Input file '" & File_Name & "' not found.");
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "An unexpected error occurred: " & Ada.Exceptions.Exception_Name(E));

end Main;
