
#!/usr/bin/perl
use strict;
use warnings;

sub concat {
    my ($a, $b) = @_;
    return $a . $b;
}

sub can_produce {
    my ($target, $nums, $idx, $value) = @_;
    return $value == $target if $idx == @$nums;
    my $n = $nums->[$idx];
    return 1 if can_produce($target, $nums, $idx + 1, $value + $n);
    return 1 if can_produce($target, $nums, $idx + 1, $value * $n);
    return 1 if can_produce($target, $nums, $idx + 1, concat($value, $n));
    return 0;
}

my $total = 0;
open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    next if $line =~ /^\s*$/;
    my ($target, $nums_str) = split /:/, $line, 2;
    $target =~ s/^\s+|\s+$//g;
    $nums_str =~ s/^\s+|\s+$//g;
    my @nums = split /\s+/, $nums_str;
    next if not @nums;
    if (@nums == 1) {
        $total += $target if $nums[0] == $target;
        next;
    }
    $total += $target if can_produce($target, \@nums, 1, $nums[0]);
}
close $fh;
print "$total\n";
