
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $sumOfSectorIDs = 0;
while (my $line = <$fh>) {
    chomp $line;
    if (isRealRoom($line)) {
        $sumOfSectorIDs += getSectorID($line);
    }
}

print "$sumOfSectorIDs\n";

sub isRealRoom {
    my ($room) = @_;
    my @parts = split /\[/, $room;
    my $checksum = substr($parts[1], 0, -1);
    my @encryptedName = split /-/, $parts[0];
    splice @encryptedName, -1;

    my %letterCounts;
    foreach my $part (@encryptedName) {
        foreach my $letter (split //, $part) {
            $letterCounts{$letter}++;
        }
    }

    my @counts;
    foreach my $letter (keys %letterCounts) {
        push @counts, { letter => $letter, count => $letterCounts{$letter} };
    }

    @counts = sort { $a->{count} == $b->{count} ? $a->{letter} cmp $b->{letter} : $b->{count} <=> $a->{count} } @counts;

    my @checksumChars = split //, $checksum;
    for my $i (0 .. $#checksumChars) {
        if ($checksumChars[$i] ne $counts[$i]{letter}) {
            return 0;
        }
    }

    return 1;
}

sub getSectorID {
    my ($room) = @_;
    my @parts = split /-/, $room;
    my $sectorIDPart = $parts[-1];
    my $sectorID = (split /\[/, $sectorIDPart)[0];
    return $sectorID;
}
