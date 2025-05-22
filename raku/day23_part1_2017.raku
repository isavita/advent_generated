
sub MAIN() {
    my @instructions = 'input.txt'.IO.lines;
    my %registers;
    my $pointer = 0;
    my $mul-count = 0;

    sub get-value($s) {
        $s.Int // %registers{$s}
    }

    while $pointer >= 0 && $pointer < @instructions.elems {
        my @parts = @instructions[$pointer].split(/\s+/);
        my ($cmd, $x, $y) = @parts;

        given $cmd {
            when "set" { %registers{$x} = get-value($y) }
            when "sub" { %registers{$x} -= get-value($y) }
            when "mul" { %registers{$x} *= get-value($y); $mul-count++ }
            when "jnz" {
                if get-value($x) != 0 {
                    $pointer += get-value($y) - 1;
                }
            }
        }
        $pointer++;
    }

    say $mul-count;
}
