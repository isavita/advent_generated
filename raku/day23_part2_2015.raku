
unit sub MAIN {
    # Read and parse the program instructions
    my @program;
    for 'input.txt'.IO.lines -> $line {
        my @parts = $line.split: ' ';
        my $op = @parts[0];
        my %instruction = :$op;

        given $op {
            when 'hlf'|'tpl'|'inc' {
                %instruction<reg> = @parts[1];
            }
            when 'jmp' {
                %instruction<offset> = @parts[1].Int;
            }
            when 'jie'|'jio' {
                # Register part includes a comma, e.g., "a,"
                %instruction<reg> = @parts[1].chop; # Remove the trailing comma
                %instruction<offset> = @parts[2].Int;
            }
        }
        @program.push: %instruction;
    }

    # --- Part One ---
    my $b-final-part1 = run-program(@program);
    say "Part 1: Final value of register b is $b-final-part1";

    # --- Part Two ---
    my $b-final-part2 = run-program(@program, a-start => 1);
    say "Part 2: Final value of register b is $b-final-part2";
}

# Subroutine to simulate the program execution
sub run-program(@program, :$a-start = 0, :$b-start = 0) {
    my %registers = 'a' => $a-start, 'b' => $b-start;
    my $ip = 0; # Instruction pointer

    # Execute instructions until the instruction pointer is out of bounds
    while $ip >= 0 && $ip < @program.elems {
        my %instruction = @program[$ip];
        my $op = %instruction<op>;

        given $op {
            when 'hlf' {
                %registers{%instruction<reg>} /= 2;
                $ip++;
            }
            when 'tpl' {
                %registers{%instruction<reg>} *= 3;
                $ip++;
            }
            when 'inc' {
                %registers{%instruction<reg>}++;
                $ip++;
            }
            when 'jmp' {
                $ip += %instruction<offset>;
            }
            when 'jie' {
                if %registers{%instruction<reg>} %% 2 { # Check if register is even
                    $ip += %instruction<offset>;
                } else {
                    $ip++;
                }
            }
            when 'jio' {
                 if %registers{%instruction<reg>} == 1 { # Check if register is one
                    $ip += %instruction<offset>;
                } else {
                    $ip++;
                }
            }
            default {
                # Should not happen with valid input
                die "Unknown instruction: $op";
            }
        }
    }

    # Return the final value of register b
    return %registers{'b'};
}
