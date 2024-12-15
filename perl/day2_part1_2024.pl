
#!/usr/bin/perl
use strict;
use warnings;

my $safe_report_count = 0;

open my $fh, '<', 'input.txt' or die "Could not open input.txt: $!";

while (my $line = <$fh>) {
    chomp $line;
    my @levels = split ' ', $line;
    
    if (@levels < 2) {
        next;
    }

    my $first_diff = $levels[1] - $levels[0];
    if ($first_diff == 0) {
        next;
    }

    my $is_increasing = $first_diff > 0;
    my $safe = 1;

    for (my $i = 0; $i < @levels - 1; $i++) {
        my $diff = $levels[$i+1] - $levels[$i];
        if ($diff == 0) {
            $safe = 0;
            last;
        }
        if (($is_increasing && $diff <= 0) || (!$is_increasing && $diff >= 0)) {
            $safe = 0;
            last;
        }
        my $abs_diff = abs($diff);
        if ($abs_diff < 1 || $abs_diff > 3) {
            $safe = 0;
            last;
        }
    }
    $safe_report_count++ if $safe;
}

close $fh;
print "$safe_report_count\n";
