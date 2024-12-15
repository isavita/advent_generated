
#!/usr/bin/perl
use strict;
use warnings;

sub is_safe_report {
    my @levels = @_;
    return 0 if @levels < 2;
    my $first_diff = $levels[1] - $levels[0];
    return 0 if $first_diff == 0;
    my $is_increasing = $first_diff > 0;
    for my $i (0 .. $#levels - 1) {
        my $diff = $levels[$i+1] - $levels[$i];
        return 0 if $diff == 0;
        return 0 if ($is_increasing && $diff <= 0) || (!$is_increasing && $diff >= 0);
        my $abs_diff = abs($diff);
        return 0 if $abs_diff < 1 || $abs_diff > 3;
    }
    return 1;
}

sub is_safe_with_one_removal {
    my @levels = @_;
    for my $i (0 .. $#levels) {
        my @modified_levels = (@levels[0..$i-1], @levels[$i+1..$#levels]);
        return 1 if is_safe_report(@modified_levels);
    }
    return 0;
}

my $safe_report_count = 0;
open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    my @levels = split ' ', $line;
    if (is_safe_report(@levels) || is_safe_with_one_removal(@levels)) {
        $safe_report_count++;
    }
}
close $fh;
print "$safe_report_count\n";
