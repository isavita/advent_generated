
sub MAIN() {
    my @directions = 'input.txt'.IO.slurp.trim.split(',');

    my $dx = 0;
    my $dy = 0;
    my $max_dist = 0;

    for @directions -> $direction {
        given $direction {
            when "n"  { $dy++ }
            when "s"  { $dy-- }
            when "ne" { $dx++ }
            when "sw" { $dx-- }
            when "nw" { $dx--; $dy++ }
            when "se" { $dx++; $dy-- }
        }
        $max_dist = max($max_dist, max($dx.abs, $dy.abs));
    }

    say max($dx.abs, $dy.abs);
    say $max_dist;
}
