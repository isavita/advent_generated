#!/usr/bin/perl
use strict;
use warnings;
open my $fh, '<', 'input.txt' or die $!;
my @ranges;
while (my $line = <$fh>) {
    chomp $line;
    $line =~ s/^\s+|\s+$//g;
    last if $line eq '';
    my ($a,$b) = split /-/, $line;
    die "Invalid range" unless defined $a && defined $b;
    $a = int $a;
    $b = int $b;
    ($a,$b) = ($b,$a) if $a > $b;
    push @ranges, [$a,$b];
}
close $fh;
if (!@ranges) {
    print "Total fresh IDs: 0\n";
    exit;
}
@ranges = sort { $a->[0] <=> $b->[0] } @ranges;
my ($cur_min,$cur_max) = @{$ranges[0]};
my $total = 0;
for my $r (@ranges[1..$#ranges]) {
    my ($min,$max) = @$r;
    if ($min <= $cur_max) {
        $cur_max = $max if $max > $cur_max;
    } else {
        $total += $cur_max - $cur_min + 1;
        ($cur_min,$cur_max) = ($min,$max);
    }
}
$total += $cur_max - $cur_min + 1;
print "Total fresh IDs: $total\n";