
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
    my @lengths;
    foreach my $char (split(//, $input)) {
        push @lengths, ord($char);
    }
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

    my $hexHash = '';
    foreach my $v (@denseHash) {
        $hexHash .= sprintf("%02x", $v);
    }
    return $hexHash;
}

sub hexToBinary {
    my ($hexStr) = @_;
    my $binaryStr = '';
    foreach my $hexDigit (split(//, $hexStr)) {
        my $val = hex($hexDigit);
        $binaryStr .= sprintf("%.4b", $val);
    }
    return $binaryStr;
}

sub dfs {
    my ($x, $y, $grid) = @_;
    if ($x < 0 || $x >= 128 || $y < 0 || $y >= 128 || $grid->[$x][$y] != 1) {
        return;
    }
    $grid->[$x][$y] = 0;
    dfs($x-1, $y, $grid);
    dfs($x+1, $y, $grid);
    dfs($x, $y-1, $grid);
    dfs($x, $y+1, $grid);
}

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
my $keyString = <$fh>;
chomp($keyString);
close($fh);

my @grid;
my $totalUsed = 0;
my $regions = 0;

for (my $i = 0; $i < 128; $i++) {
    my $rowKey = $keyString . "-" . $i;
    my $hash = knotHash($rowKey);
    my $binaryRow = hexToBinary($hash);

    my @row;
    foreach my $bit (split(//, $binaryRow)) {
        if ($bit eq '1') {
            push @row, 1;
            $totalUsed++;
        } else {
            push @row, 0;
        }
    }
    push @grid, \@row;
}

for (my $i = 0; $i < 128; $i++) {
    for (my $j = 0; $j < 128; $j++) {
        if ($grid[$i][$j] == 1) {
            $regions++;
            dfs($i, $j, \@grid);
        }
    }
}

print "$regions\n";
