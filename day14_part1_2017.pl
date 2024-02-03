
use strict;
use warnings;
use Digest::MD5 qw(md5_hex);

sub reverseSection {
    my ($arr, $start, $length) = @_;
    my $n = scalar(@$arr);
    for (my $i = $start, my $j = $start + $length - 1; $i < $j; $i++, $j--) {
        ($arr->[$i % $n], $arr->[$j % $n]) = ($arr->[$j % $n], $arr->[$i % $n]);
    }
}

sub knotHash {
    my ($input) = @_;
    my @lengths = map { ord($_) } split("", $input);
    push @lengths, (17, 31, 73, 47, 23);

    my @list = (0..255);

    my ($position, $skip) = (0, 0);
    for (my $round = 0; $round < 64; $round++) {
        foreach my $length (@lengths) {
            reverseSection(\@list, $position, $length);
            $position += $length + $skip;
            $skip++;
        }
    }

    my @denseHash;
    for (my $i = 0; $i < 16; $i++) {
        my $xor = 0;
        for (my $j = 0; $j < 16; $j++) {
            $xor ^= $list[$i * 16 + $j];
        }
        push @denseHash, $xor;
    }

    my $hexHash = "";
    foreach my $v (@denseHash) {
        $hexHash .= sprintf("%02x", $v);
    }
    return $hexHash;
}

sub hexToBinary {
    my ($hexStr) = @_;
    my $binaryStr = "";
    foreach my $hexDigit (split("", $hexStr)) {
        my $val = hex($hexDigit);
        $binaryStr .= sprintf("%.4b", $val);
    }
    return $binaryStr;
}

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $keyString = <$fh>;
close($fh);

chomp($keyString);
my $totalUsed = 0;

for (my $i = 0; $i < 128; $i++) {
    my $rowKey = $keyString . "-" . $i;
    my $hash = knotHash($rowKey);
    my $binaryRow = hexToBinary($hash);

    foreach my $bit (split("", $binaryRow)) {
        if ($bit eq '1') {
            $totalUsed++;
        }
    }
}

print "$totalUsed\n";
