
use strict;
use warnings;
use Digest::MD5 qw(md5_hex);

open(my $fh, '<', "input.txt") or die "Cannot open file: $!";
my $input = <$fh>;
close($fh);

my @lengths = map { ord($_) } split('', $input);
push @lengths, (17, 31, 73, 47, 23);

my @list = (0..255);
my $currentPosition = 0;
my $skipSize = 0;

for (my $round = 0; $round < 64; $round++) {
    foreach my $length (@lengths) {
        for (my $i = 0; $i < $length/2; $i++) {
            my $start = ($currentPosition + $i) % 256;
            my $end = ($currentPosition + $length - 1 - $i) % 256;
            ($list[$start], $list[$end]) = ($list[$end], $list[$start]);
        }
        $currentPosition = ($currentPosition + $length + $skipSize) % 256;
        $skipSize++;
    }
}

my @denseHash;
for (my $i = 0; $i < 256; $i += 16) {
    my $xor = 0;
    for (my $j = 0; $j < 16; $j++) {
        $xor ^= $list[$i+$j];
    }
    push @denseHash, $xor;
}

my $hexHash = unpack('H*', pack('C*', @denseHash));
print "$hexHash\n";
