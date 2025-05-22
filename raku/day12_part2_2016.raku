
sub get-value($s, %registers) {
    $s.Int // %registers{$s}
}

sub execute-instructions(@instructions, %registers) {
    my $i = 0;
    while $i < @instructions.elems {
        my @parts = @instructions[$i].split(' ');
        given @parts[0] {
            when 'cpy' {
                %registers{@parts[2]} = get-value(@parts[1], %registers);
                $i++;
            }
            when 'inc' {
                %registers{@parts[1]}++;
                $i++;
            }
            when 'dec' {
                %registers{@parts[1]}--;
                $i++;
            }
            when 'jnz' {
                if get-value(@parts[1], %registers) != 0 {
                    $i += @parts[2].Int;
                } else {
                    $i++;
                }
            }
        }
    }
}

sub MAIN() {
    my @instructions = "input.txt".IO.slurp(:chomp).lines;

    my %registers = <a b c d> Z=> 0;
    %registers<c> = 1;

    execute-instructions(@instructions, %registers);

    %registers<a>.say;
}
