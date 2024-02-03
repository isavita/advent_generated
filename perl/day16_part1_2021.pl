
use strict;
use warnings;

sub hexToBin {
    my $hex = shift;
    my $bin = "";
    foreach my $h (split //, $hex) {
        my $b = hex($h);
        $bin .= sprintf("%04b", $b);
    }
    return $bin;
}

sub parsePacket {
    my ($binStr, $idx) = @_;
    my $version = int(substr($binStr, $idx, 1) << 2 | substr($binStr, $idx + 1, 1) << 1 | substr($binStr, $idx + 2, 1));
    my $typeID = int(substr($binStr, $idx + 3, 1) << 2 | substr($binStr, $idx + 4, 1) << 1 | substr($binStr, $idx + 5, 1));
    $idx += 6;

    if ($typeID == 4) {
        while (substr($binStr, $idx, 1) == '1') {
            $idx += 5;
        }
        $idx += 5;
        return ($version, $idx);
    }

    my $lengthTypeID = int(substr($binStr, $idx, 1));
    $idx++;
    my ($numSubPackets, $subPacketLength);

    if ($lengthTypeID == 0) {
        $subPacketLength = 0;
        for (my $i = 0; $i < 15; $i++) {
            $subPacketLength = $subPacketLength << 1 | substr($binStr, $idx, 1);
            $idx++;
        }
    } else {
        $numSubPackets = 0;
        for (my $i = 0; $i < 11; $i++) {
            $numSubPackets = $numSubPackets << 1 | substr($binStr, $idx, 1);
            $idx++;
        }
    }

    my $versionSum = $version;
    while (1) {
        if ($lengthTypeID == 0 && $subPacketLength == 0) {
            last;
        }
        if ($lengthTypeID == 1 && $numSubPackets == 0) {
            last;
        }
        my ($subVersion, $newIndex) = parsePacket($binStr, $idx);
        $versionSum += $subVersion;

        if ($lengthTypeID == 0) {
            $subPacketLength -= $newIndex - $idx;
        } else {
            $numSubPackets--;
        }
        $idx = $newIndex;
    }
    return ($versionSum, $idx);
}

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $hexStr = <$fh>;
close($fh);

chomp($hexStr);
my $binStr = hexToBin($hexStr);
my ($versionSum, $idx) = parsePacket($binStr, 0);
print "$versionSum\n";
