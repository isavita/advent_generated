
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my @lines;
while (my $line = <$fh>) {
    chomp $line;
    push @lines, $line;
}
close($fh);

for (my $i = 0; $i < scalar(@lines)-1; $i++) {
    for (my $j = $i + 1; $j < scalar(@lines); $j++) {
        my $diff = 0;
        for (my $k = 0; $k < length($lines[$i]); $k++) {
            if (substr($lines[$i], $k, 1) ne substr($lines[$j], $k, 1)) {
                $diff++;
                if ($diff > 1) {
                    last;
                }
            }
        }
        if ($diff == 1) {
            my $common = "";
            for (my $k = 0; $k < length($lines[$i]); $k++) {
                if (substr($lines[$i], $k, 1) eq substr($lines[$j], $k, 1)) {
                    $common .= substr($lines[$i], $k, 1);
                }
            }
            print "$common\n";
            exit;
        }
    }
}
