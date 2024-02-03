
use strict;
use warnings;

my %mfcsam = (
    "children" => 3, "cats" => 7, "samoyeds" => 2, "pomeranians" => 3,
    "akitas" => 0, "vizslas" => 0, "goldfish" => 5, "trees" => 3,
    "cars" => 2, "perfumes" => 1,
);

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(' ', $line);
    my $sueNumber = substr($parts[1], 0, -1);

    my $matches = 1;
    for (my $i = 2; $i < scalar @parts; $i += 2) {
        my $item = substr($parts[$i], 0, -1);
        my $count = substr($parts[$i + 1], 0, -1);
        if ($mfcsam{$item} != $count) {
            $matches = 0;
            last;
        }
    }

    if ($matches) {
        print "$sueNumber\n";
        last;
    }
}
close($fh) or die "Error closing file: $!";
