
my $file = 'input.txt';
open(my $fh, '<', $file) or die "Could not open file '$file' $!";
my $available_line = <$fh>;
chomp $available_line;
my @available_patterns = map { s/^\s+|\s+$//gr } split ',', $available_line;
my $blank_line = <$fh>;
my $count = 0;

while (my $design = <$fh>) {
    chomp $design;
    my $n = length $design;
    my @dp = (0) x ($n + 1);
    $dp[0] = 1;
    for my $i (1 .. $n) {
        for my $p (@available_patterns) {
            my $lp = length $p;
            if ($i >= $lp && $dp[$i - $lp] && substr($design, $i - $lp, $lp) eq $p) {
                $dp[$i] = 1;
                last;
            }
        }
    }
    $count++ if $dp[$n];
}

close $fh;
print $count;

