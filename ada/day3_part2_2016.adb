
-- Standard Ada libraries for input/output operations
with Ada.Text_IO;
with Ada.Integer_Text_IO;

-- Main procedure for the program.
-- This procedure serves as the entry point for execution.
procedure Solve_Day3 is

   -- File handle for interacting with 'input.txt'
   Input_File : Ada.Text_IO.File_Type;

   -- Counters to store the number of valid triangles for each part of the problem
   Valid_Horizontal_Triangles : Natural := 0; -- Natural type ensures non-negative count
   Valid_Vertical_Triangles   : Natural := 0;

   -- Function to determine if three given side lengths can form a valid triangle.
   -- A triangle is valid if the sum of any two sides is strictly greater than the third side.
   function Is_Valid_Triangle(S1, S2, S3 : Integer) return Boolean is
   begin
      -- Apply the triangle inequality theorem
      return (S1 + S2 > S3) and then
             (S1 + S3 > S2) and then
             (S2 + S3 > S1);
   end Is_Valid_Triangle;

begin
   -- --- Part 1: Count horizontal triangles ---
   -- Each line in the input file represents a potential triangle.

   -- Open the input file in read mode.
   -- If 'input.txt' does not exist, a Name_Error exception will be raised.
   Ada.Text_IO.Open(Input_File, Ada.Text_IO.In_File, "input.txt");

   -- Loop to read and process each line of the file.
   -- The loop continues until an End_Error is raised, indicating the end of the file.
   loop
      declare
         -- Variables to hold the three side lengths read from a line
         A, B, C : Integer;
      begin
         -- Attempt to read three integers from the current position in the file.
         -- Ada.Integer_Text_IO.Get automatically skips leading whitespace, including newlines,
         -- making it suitable for reading numbers separated by spaces and newlines.
         Ada.Integer_Text_IO.Get(Input_File, A);
         Ada.Integer_Text_IO.Get(Input_File, B);
         Ada.Integer_Text_IO.Get(Input_File, C);

         -- Check if the read numbers form a valid triangle using our helper function.
         if Is_Valid_Triangle(A, B, C) then
            Valid_Horizontal_Triangles := Valid_Horizontal_Triangles + 1;
         end if;
      exception
         when Ada.Text_IO.End_Error =>
            -- If End_Error is caught, it means we've reached the end of the file
            -- (or an incomplete set of numbers at the very end).
            -- Exit the loop as there are no more full sets of three numbers to read.
            exit;
      end;
   end loop;

   -- Print the result for Part 1 to standard output.
   -- Natural'Image converts the Natural count to a String for printing.
   Ada.Text_IO.Put_Line("Part 1: Valid horizontal triangles: " & Natural'Image(Valid_Horizontal_Triangles));

   -- Close the file. It's good practice to close files when done,
   -- especially before reopening for a different processing mode or from the beginning.
   Ada.Text_IO.Close(Input_File);

   -- --- Part 2: Count vertical triangles ---
   -- Triangles are formed by numbers in columns, grouped by three lines.

   -- Reopen the input file to read from the beginning for Part 2.
   Ada.Text_IO.Open(Input_File, Ada.Text_IO.In_File, "input.txt");

   -- Loop to process the file in groups of three lines.
   loop
      declare
         -- Variables to store the nine numbers from a group of three lines.
         -- L1_A, L1_B, L1_C are from the first line of the group, etc.
         L1_A, L1_B, L1_C : Integer;
         L2_A, L2_B, L2_C : Integer;
         L3_A, L3_B, L3_C : Integer;
      begin
         -- Read the first line of the current group
         Ada.Integer_Text_IO.Get(Input_File, L1_A);
         Ada.Integer_Text_IO.Get(Input_File, L1_B);
         Ada.Integer_Text_IO.Get(Input_File, L1_C);

         -- Read the second line of the current group
         Ada.Integer_Text_IO.Get(Input_File, L2_A);
         Ada.Integer_Text_IO.Get(Input_File, L2_B);
         Ada.Integer_Text_IO.Get(Input_File, L2_C);

         -- Read the third line of the current group
         Ada.Integer_Text_IO.Get(Input_File, L3_A);
         Ada.Integer_Text_IO.Get(Input_File, L3_B);
         Ada.Integer_Text_IO.Get(Input_File, L3_C);

         -- Now, form and check the three vertical triangles:
         -- Triangle 1: composed of the first numbers from each of the three lines
         if Is_Valid_Triangle(L1_A, L2_A, L3_A) then
            Valid_Vertical_Triangles := Valid_Vertical_Triangles + 1;
         end if;

         -- Triangle 2: composed of the second numbers from each of the three lines
         if Is_Valid_Triangle(L1_B, L2_B, L3_B) then
            Valid_Vertical_Triangles := Valid_Vertical_Triangles + 1;
         end if;

         -- Triangle 3: composed of the third numbers from each of the three lines
         if Is_Valid_Triangle(L1_C, L2_C, L3_C) then
            Valid_Vertical_Triangles := Valid_Vertical_Triangles + 1;
         end if;

      exception
         when Ada.Text_IO.End_Error =>
            -- If End_Error is caught here, it means the file ended while trying to read
            -- the second or third line of a group, indicating an incomplete group.
            -- We exit the loop as these incomplete groups cannot form valid vertical triangles.
            exit;
      end;
   end loop;

   -- Print the result for Part 2 to standard output.
   Ada.Text_IO.Put_Line("Part 2: Valid vertical triangles: " & Natural'Image(Valid_Vertical_Triangles));

   -- Close the file after all processing is complete.
   Ada.Text_IO.Close(Input_File);

exception
   -- Exception handler for when 'input.txt' cannot be found or accessed.
   when Ada.Text_IO.Name_Error =>
      Ada.Text_IO.Put_Line("Error: input.txt not found. Please ensure the file exists in the same directory.");
   -- Generic exception handler for any other unexpected errors during execution.
   when others =>
      Ada.Text_IO.Put_Line("An unexpected error occurred during file processing or calculation.");

end Solve_Day3;
