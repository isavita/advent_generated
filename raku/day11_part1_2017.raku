
sub MAIN() {
    my @directions = 'input.txt'.IO.slurp.trim.split(',');

    my ($x, $y, $z) = 0, 0, 0;

    for @directions -> $direction {
        given $direction {
            when "n"  { $y++; $z-- }
            when "ne" { $x++; $z-- }
            when "se" { $x++; $y-- }
            when "s"  { $y--; $z++ }
            when "sw" { $x--; $z++ }
            when "nw" { $x--; $y++ }
        }
    }

    say ($x.abs, $y.abs, $z.abs).max;
}
