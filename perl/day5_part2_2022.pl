
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = do { local $/; <$fh> };
close($fh);

my @s = split("\n\n", $input);
my @input = split("\n", $s[0]);
my @stacks;
for my $line (@input) {
    my @chars = split('', $line);
    for my $i (0..$#chars) {
        if ($chars[$i] ge 'A' && $chars[$i] le 'Z') {
            push @{$stacks[($i-1)/4]}, $chars[$i];
        }
    }
}

my @steps = split("\n", $s[1]);
print move(\@stacks, \@steps);

sub move {
    my ($st, $steps) = @_;
    my @stacks;
    for my $i (0..$#{$st}) {
        @{$stacks[$i]} = reverse @{$st->[$i]};
    }

    for my $step (@$steps) {
        my ($n, $from, $to);
        if ($step =~ /move (\d+) from (\d+) to (\d+)/) {
            ($n, $from, $to) = ($1, $2-1, $3-1);
            push @{$stacks[$to]}, splice(@{$stacks[$from]}, -$n);
        }
    }

    my $result = '';
    for my $i (0..$#stacks) {
        $result .= $stacks[$i][-1];
    }
    return $result;
}
