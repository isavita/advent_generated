
/* REXX */
/*------------------------------------------------------------------*/
/* Program: Day 15: Science for Hungry People                       */
/*                                                                  */
/* Description: This program solves the Advent of Code Day 15       */
/*              challenge. It calculates the optimal ingredient mix  */
/*              for a cookie recipe based on specific properties.    */
/*                                                                  */
/* Input:  A file named 'input.txt' with ingredient properties.     */
/*         Each line should be in the format:                       */
/*         Name: capacity C, durability D, flavor F, texture T, cal X */
/*                                                                  */
/* Output: The highest possible cookie score for two scenarios:     */
/*         1. Any calorie count.                                    */
/*         2. Exactly 500 calories.                                 */
/*------------------------------------------------------------------*/
options levelb

/**********************************************************************/
/* Main entry point                                                   */
/**********************************************************************/
main:
    /* Define the input file name */
    filename = 'input.txt'

    /* Initialize the ingredient stem variable */
    ing. = 0

    /* Parse the input file to populate the 'ing.' stem */
    call parse_input filename

    /* If no ingredients were found, display an error and exit */
    if ing.0 = 0 then do
        say "Error: Input file '"filename"' not found or is empty."
        return 1
    end

    /* Solve the problem for both parts */
    call solve

exit 0

/**********************************************************************/
/* Subroutine: parse_input                                            */
/* Description: Reads and parses ingredient data from the given file. */
/* Arguments: 1. filename - The name of the file to read.           */
/* Exposes: ing. - The stem variable holding ingredient properties.   */
/**********************************************************************/
parse_input:
    procedure expose ing.
    arg filename

    /* Check if the file exists before attempting to read */
    if stream(filename, 'c', 'query exists') = '' then return

    i = 0
    /* Read the file line by line until the end */
    do while lines(filename) > 0
        line = linein(filename)
        i = i + 1

        /* Parse the line into properties. Example: */
        /* Sprinkles: capacity 5, durability -1, flavor 0, ... */
        parse var line ing.i.name ':' . ,
            'capacity' ing.i.cap ',' ,
            'durability' ing.i.dur ',' ,
            'flavor' ing.i.flav ',' ,
            'texture' ing.i.tex ',' ,
            'calories' ing.i.cal
    end
    ing.0 = i /* Store the total count of ingredients */

    call lineout filename /* Close the file stream */
return

/**********************************************************************/
/* Subroutine: solve                                                  */
/* Description: Finds the highest scores by iterating through all     */
/*              valid combinations of ingredients.                    */
/* Exposes: ing. - The stem variable holding ingredient properties.   */
/**********************************************************************/
solve:
    procedure expose ing.

    /* This solution is optimized for 4 ingredients, which is the    */
    /* number in the puzzle's input. A general solution for N        */
    /* ingredients would require a more complex recursive approach.  */
    if ing.0 \= 4 then do
        say "Error: This solution is designed for exactly 4 ingredients."
        say "Found" ing.0 "ingredients in the input file."
        return
    end

    max_score_p1 = 0
    max_score_p2 = 0
    total_teaspoons = 100

    /* Loop through all combinations of 4 ingredients that sum to 100 */
    do t1 = 0 to total_teaspoons
        do t2 = 0 to total_teaspoons - t1
            do t3 = 0 to total_teaspoons - t1 - t2
                t4 = total_teaspoons - t1 - t2 - t3

                /* Store teaspoon amounts in a temporary stem for easier access */
                t.1 = t1; t.2 = t2; t.3 = t3; t.4 = t4

                /* Calculate total properties for the current combination */
                capacity   = 0
                durability = 0
                flavor     = 0
                texture    = 0
                calories   = 0

                do i = 1 to ing.0
                    capacity   = capacity   + t.i * ing.i.cap
                    durability = durability + t.i * ing.i.dur
                    flavor     = flavor     + t.i * ing.i.flav
                    texture    = texture    + t.i * ing.i.tex
                    calories   = calories   + t.i * ing.i.cal
                end

                /* Property scores cannot be negative; set to 0 if they are */
                if capacity   < 0 then capacity   = 0
                if durability < 0 then durability = 0
                if flavor     < 0 then flavor     = 0
                if texture    < 0 then texture    = 0

                /* Calculate the total score for this combination */
                score = capacity * durability * flavor * texture

                /* Part 1: Track the overall maximum score */
                if score > max_score_p1 then max_score_p1 = score

                /* Part 2: Track the maximum score for 500-calorie cookies */
                if calories = 500 then do
                    if score > max_score_p2 then max_score_p2 = score
                end
            end
        end
    end

    /* Print the final results */
    say "--- Day 15: Science for Hungry People ---"
    say ""
    say "Part 1: The total score of the highest-scoring cookie is" max_score_p1
    say "Part 2: The total score of the highest-scoring 500-calorie cookie is" max_score_p2
return
