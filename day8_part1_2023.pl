
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $input = do { local $/; <$fh> };
close($fh);

my $ElemToMatch = "ZZZ";

my %desertMap;

my @lines = split("\n", $input);

# Ignore the first two lines since they are the instruction set and a blank line
for my $i (2..$#lines) {
    next if $lines[$i] eq "";

    my @matches = $lines[$i] =~ /[A-Z]{3}/g;
    $desertMap{$matches[0]} = { left => $matches[1], right => $matches[2] };
}

my $current = "AAA";
my $steps = 0;

while ($current ne $ElemToMatch) {
    my @directions = split("", $lines[0]);

    for my $i (0..$#directions) {
        my $direction = $directions[$i];
        if ($direction eq 'R') {
            $current = $desertMap{$current}{right};
        } else {
            $current = $desertMap{$current}{left};
        }
        $steps++;

        last if $current eq $ElemToMatch;
    }
}

print "$steps\n";
