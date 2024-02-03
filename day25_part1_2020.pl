
use strict;
use warnings;

sub transform {
    my ($subjectNumber, $loopSize) = @_;
    my $value = 1;
    for (my $i = 0; $i < $loopSize; $i++) {
        $value *= $subjectNumber;
        $value %= 20201227;
    }
    return $value;
}

sub findLoopSize {
    my ($publicKey) = @_;
    my $value = 1;
    my $loopSize = 0;
    while ($value != $publicKey) {
        $value *= 7;
        $value %= 20201227;
        $loopSize++;
    }
    return $loopSize;
}

open(my $fh, '<', 'input.txt') or die $!;
my $cardPublicKey = <$fh>;
my $doorPublicKey = <$fh>;
chomp($cardPublicKey);
chomp($doorPublicKey);

my $cardLoopSize = findLoopSize($cardPublicKey);
my $encryptionKey = transform($doorPublicKey, $cardLoopSize);

print "$encryptionKey\n";
