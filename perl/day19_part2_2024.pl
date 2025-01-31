
open my $f, '<', 'input.txt';
my $available_line = <$f>;
chomp $available_line;
my @available_patterns = map { s/^\s+|\s+$//gr } split ',', $available_line;
<$f>;
my $total_ways = 0;

while (my $design = <$f>) {
    chomp $design;
    my $n = length $design;
    my @dp = (0) x ($n + 1);
    $dp[0] = 1;
    for my $i (1 .. $n) {
        for my $p (@available_patterns) {
            my $lp = length $p;
            if ($i >= $lp && substr($design, $i - $lp, $lp) eq $p) {
                $dp[$i] += $dp[$i - $lp];
            }
        }
    }
    $total_ways += $dp[$n];
}
close $f;
print $total_ways;

