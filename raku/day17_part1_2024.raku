
sub MAIN {
    my ($A, $B, $C) = 0, 0, 0;
    my @program;
    my @output-vals;
    my $ip = 0;

    for 'input.txt'.IO.lines -> $line {
        my $s = $line.trim;
        next unless $s.chars;

        if $s.starts-with('Register A:') {
            $A = $s.split(':')[1].trim.Int;
        } elsif $s.starts-with('Register B:') {
            $B = $s.split(':')[1].trim.Int;
        } elsif $s.starts-with('Register C:') {
            $C = $s.split(':')[1].trim.Int;
        } elsif $s.starts-with('Program:') {
            @program = $s.split(':')[1].trim.split(',').map(*.trim.Int);
        }
    }

    my &getComboVal = -> $op {
        given $op {
            when 0|1|2|3 { $op }
            when 4 { $A }
            when 5 { $B }
            when 6 { $C }
            default { die "invalid combo operand" }
        }
    };

    loop {
        last if $ip >= @program.elems;
        my $opcode = @program[$ip];

        last if $ip + 1 >= @program.elems;
        my $operand = @program[$ip+1];

        given $opcode {
            when 0 {
                my $den = getComboVal($operand);
                $A = $den == 0 ?? 0 !! $A div (2 ** $den);
                $ip += 2;
            }
            when 1 {
                $B = $B +^ $operand;
                $ip += 2;
            }
            when 2 {
                $B = getComboVal($operand) % 8;
                $ip += 2;
            }
            when 3 {
                if $A != 0 {
                    $ip = $operand;
                } else {
                    $ip += 2;
                }
            }
            when 4 {
                $B = $B +^ $C;
                $ip += 2;
            }
            when 5 {
                @output-vals.push: (getComboVal($operand) % 8).Str;
                $ip += 2;
            }
            when 6 {
                my $den = getComboVal($operand);
                $B = $A div (2 ** $den);
                $ip += 2;
            }
            when 7 {
                my $den = getComboVal($operand);
                $C = $A div (2 ** $den);
                $ip += 2;
            }
            default {
                last;
            }
        }
    }

    @output-vals.join(',').say;
}
