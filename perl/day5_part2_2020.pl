
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @seatIDs;

while (my $pass = <$fh>) {
    chomp $pass;
    $pass =~ s/F/0/g;
    $pass =~ s/B/1/g;
    $pass =~ s/L/0/g;
    $pass =~ s/R/1/g;
    my $seatID = decode($pass);
    push @seatIDs, $seatID;
}

sub decode {
    my ($pass) = @_;
    my $row = binaryToInt(substr($pass, 0, 7));
    my $column = binaryToInt(substr($pass, 7));
    return $row * 8 + $column;
}

sub binaryToInt {
    my ($binaryStr) = @_;
    my $result = 0;
    my @chars = split('', $binaryStr);
    foreach my $i (0..$#chars) {
        if ($chars[$i] eq '1') {
            $result |= 1 << (length($binaryStr) - $i - 1);
        }
    }
    return $result;
}

@seatIDs = sort { $a <=> $b } @seatIDs;

for my $i (0..$#seatIDs-1) {
    if ($seatIDs[$i+1] != $seatIDs[$i] + 1) {
        print $seatIDs[$i] + 1, "\n";
        last;
    }
}
