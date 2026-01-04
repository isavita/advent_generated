
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(product);

open my $fh, '<', 'input.txt' or die $!;
my @pts;
while (<$fh>) {
    chomp;
    next unless $_;
    my ($x,$y,$z) = split /,/;
    push @pts, [$x+0,$y+0,$z+0];
}
close $fh;
my $n = @pts;
exit if $n < 2;

my @edges;
for my $i (0..$n-2) {
    my ($xi,$yi,$zi) = @{$pts[$i]};
    for my $j ($i+1..$n-1) {
        my ($xj,$yj,$zj) = @{$pts[$j]};
        my $dx = $xi-$xj; my $dy = $yi-$yj; my $dz = $zi-$zj;
        my $d  = $dx*$dx + $dy*$dy + $dz*$dz;
        push @edges, [$d,$i,$j];
    }
}
@edges = sort { $a->[0] <=> $b->[0] } @edges;

my @parent = (0..$n-1);
my @size   = (1) x $n;
sub find {
    my ($i) = @_;
    $parent[$i] = find($parent[$i]) if $parent[$i] != $i;
    $parent[$i];
}
sub union {
    my ($a,$b) = @_;
    my $ra = find($a);
    my $rb = find($b);
    return if $ra == $rb;
    if ($size[$ra] < $size[$rb]) { ($ra,$rb) = ($rb,$ra) }
    $parent[$rb] = $ra;
    $size[$ra] += $size[$rb];
}

my $limit = @edges < 1000 ? @edges : 1000;
for my $k (0..$limit-1) {
    union($edges[$k][1], $edges[$k][2]);
}

my @circuit_sizes;
for my $i (0..$n-1) {
    push @circuit_sizes, $size[$i] if $parent[$i] == $i;
}
@circuit_sizes = sort { $b <=> $a } @circuit_sizes;
my $result = product(@circuit_sizes[0..2]) // 1;
print "Product of three largest circuit sizes: $result\n";
