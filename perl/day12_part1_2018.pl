
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $initialState;
my %rules;

while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ /initial state/) {
        $initialState = (split /: /, $line)[1];
    } elsif ($line =~ /=>/) {
        my @parts = split / => /, $line;
        $rules{$parts[0]} = substr($parts[1], 0, 1);
    }
}

my %state;
for my $i (0 .. length($initialState) - 1) {
    my $c = substr($initialState, $i, 1);
    $state{$i} = '#' if $c eq '#';
}

for my $generation (0 .. 19) {
    my %newState;
    my ($minPot, $maxPot) = minMaxKeys(%state);
    for my $i ($minPot - 2 .. $maxPot + 2) {
        my $pattern = "";
        for my $j ($i - 2 .. $i + 2) {
            if ($state{$j} && $state{$j} eq '#') {
                $pattern .= "#";
            } else {
                $pattern .= ".";
            }
        }
        if ($rules{$pattern} && $rules{$pattern} eq '#') {
            $newState{$i} = '#';
        }
    }
    %state = %newState;
}

my $sum = 0;
for my $k (keys %state) {
    $sum += $k;
}

print "$sum\n";

sub minMaxKeys {
    my %m = @_;
    my $first = 1;
    my ($minKey, $maxKey);
    for my $k (keys %m) {
        if ($first) {
            $minKey = $k;
            $maxKey = $k;
            $first = 0;
        } else {
            $minKey = $k if $k < $minKey;
            $maxKey = $k if $k > $maxKey;
        }
    }
    return ($minKey, $maxKey);
}
