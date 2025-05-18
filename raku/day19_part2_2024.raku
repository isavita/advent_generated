
sub count-ways(Str $design, @patterns) {
    my $n = $design.chars;
    my @dp = (0) xx ($n + 1);
    @dp[0] = 1;

    for 1 .. $n -> $i {
        for @patterns -> $p {
            my $lp = $p.chars;
            if $i >= $lp && $design.substr($i - $lp, $lp) eq $p {
                @dp[$i] += @dp[$i - $lp];
            }
        }
    }
    return @dp[$n];
}

sub MAIN {
    my $file = 'input.txt'.IO;
    my @lines = $file.lines;

    my @available-patterns = @lines[0].split(',').map({ .trim });
    my $total-ways = 0;

    for @lines[2 .. *] -> $design {
        $total-ways += count-ways($design.trim, @available-patterns);
    }

    say $total-ways;
}
