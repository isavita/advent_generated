
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my $input = <$fh>;
close($fh);

my $decompressedLength = getDecompressedLength($input);
print $decompressedLength . "\n";

sub getDecompressedLength {
    my $input = shift;
    my $length = 0;
    while ($input =~ /\((\d+)x(\d+)\)/g) {
        my $charCount = $1;
        my $repeatCount = $2;
        $length += $charCount * $repeatCount;
        $input =~ s/\((\d+)x(\d+)\)//;
        $input =~ s/.{$charCount}//;
    }
    $length += length($input);
    return $length;
}
