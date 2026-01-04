use strict;
use warnings;
use Math::BigInt;
my @lines;
open my $fh, '<', 'input.txt' or die $!;
while (my $l = <$fh>) { chomp $l; push @lines, $l }
close $fh;
my $max = 0;
$max = length($_) > $max ? length($_) : $max for @lines;
if (!@lines) { print "Grand total: 0\n"; exit }
my @sep;
for my $x (0 .. $max-1) {
    my $all = 1;
    for my $line (@lines) {
        if ($x < length $line && substr($line,$x,1) !~ /\s/) { $all = 0; last }
    }
    $sep[$x] = $all;
}
my $grand = Math::BigInt->new(0);
my ($in,$start);
for my $x (0 .. $max-1) {
    if (!$sep[$x]) {
        unless ($in) { $in=1; $start=$x }
    } else {
        if ($in) { process($start,$x-1); $in=0 }
    }
}
process($start,$max-1) if $in;
print "Grand total: $grand\n";

sub process {
    my ($s,$e) = @_;
    my @nums;
    my $op = '';
    for my $c ($s .. $e) {
        my $sb = '';
        my $has = 0;
        for my $line (@lines) {
            next if $c >= length $line;
            my $ch = substr($line,$c,1);
            if ($ch =~ /\d/) { $sb .= $ch; $has = 1 }
            elsif ($ch eq '+' || $ch eq '*') { $op = $ch }
        }
        push @nums, Math::BigInt->new($sb) if $has;
    }
    return unless @nums;
    my $res = $op eq '*' ? Math::BigInt->new(1) : Math::BigInt->new(0);
    for my $n (@nums) {
        $op eq '*' ? $res->bmul($n) : $res->badd($n);
    }
    $grand->badd($res);
}
