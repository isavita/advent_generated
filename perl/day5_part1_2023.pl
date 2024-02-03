
use strict;
use warnings;

my @seeds;
my @currentRanges;
my @maps;

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";

while (my $line = <$fh>) {
    chomp $line;
    if ($line =~ /map:/) {
        if (@currentRanges) {
            push @maps, [@currentRanges];
            @currentRanges = ();
        }
    } elsif ($line =~ /^seeds:/) {
        my @seedStrs = split(" ", substr($line, 7));
        push @seeds, @seedStrs;
    } else {
        my @numbers = split(" ", $line);
        if (@numbers == 3) {
            my ($destStart, $srcStart, $length) = @numbers;
            push @currentRanges, { srcStart => $srcStart, destStart => $destStart, length => $length };
        }
    }
}
push @maps, [@currentRanges];

my $minLocation = -1;
foreach my $seed (@seeds) {
    my $location = $seed;
    foreach my $m (@maps) {
        $location = convertNumber($location, $m);
    }

    if ($minLocation == -1 || $location < $minLocation) {
        $minLocation = $location;
    }
}

print "$minLocation\n";

sub convertNumber {
    my ($number, $ranges) = @_;
    foreach my $r (@$ranges) {
        if ($number >= $r->{srcStart} && $number < $r->{srcStart} + $r->{length}) {
            return $r->{destStart} + ($number - $r->{srcStart});
        }
    }
    return $number;
}
