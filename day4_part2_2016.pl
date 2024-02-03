
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

while (my $line = <$fh>) {
    chomp $line;
    if (isRealRoom($line)) {
        my $decryptedName = decryptName($line);
        if ($decryptedName =~ /northpole object/) {
            print getSectorID($line) . "\n";
            last;
        }
    }
}

sub isRealRoom {
    my ($room) = @_;
    my @parts = split /\[/, $room;
    my $checksum = (split /\]/, $parts[1])[0];
    my @encryptedName = split /-/, $parts[0];
    pop @encryptedName;

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

    @counts = sort { $b->{count} <=> $a->{count} || $a->{letter} cmp $b->{letter} } @counts;

    my @checksumLetters = split //, $checksum;
    for my $i (0 .. $#checksumLetters) {
        if ($checksumLetters[$i] ne $counts[$i]{letter}) {
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

sub decryptName {
    my ($room) = @_;
    my @parts = split /-/, $room;
    my $sectorIDPart = $parts[-1];
    my $sectorID = (split /\[/, $sectorIDPart)[0];
    my $decryptedName = '';

    foreach my $part (@parts[0 .. $#parts-1]) {
        foreach my $letter (split //, $part) {
            if ($letter eq '-') {
                $decryptedName .= ' ';
            } else {
                my $shiftedLetter = chr(ord('a') + ((ord($letter) - ord('a') + $sectorID) % 26));
                $decryptedName .= $shiftedLetter;
            }
        }
        $decryptedName .= ' ';
    }

    return $decryptedName;
}
