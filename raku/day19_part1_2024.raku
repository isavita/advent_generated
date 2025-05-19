
sub MAIN {
    my $patterns;
    my $count = 0;
    my $file = open "input.txt" or die $!;
    $patterns = $file.get.split(',').map({ .trim });
    $file.get;
    for $file.lines {
        my $design = .trim;
        if can-make($design, $patterns) {
            $count++;
        }
    }
    $file.close;
    say $count;
}

sub can-make(Str $design, @patterns) {
    my $n = $design.chars;
    my @dp = False xx ($n + 1);
    @dp[0] = True;
    for 1 .. $n -> $i {
        for @patterns -> $p {
            my $lp = $p.chars;
            if $i >= $lp and @dp[$i - $lp] and $design.substr($i - $lp, $lp) eq $p {
                @dp[$i] = True;
                last;
            }
        }
    }
    return @dp[$n];
}
