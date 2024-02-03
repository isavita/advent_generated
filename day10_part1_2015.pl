
use strict;
use warnings;

my $filename = "input.txt";
open(my $fh, '<', $filename) or die "Error opening file: $!";
my $initialSequence = <$fh>;
close($fh);

my $result = lookAndSay($initialSequence, 40);
print length($result) . "\n";

sub lookAndSay {
    my ($sequence, $iterations) = @_;
    for (my $i = 0; $i < $iterations; $i++) {
        $sequence = nextSequence($sequence);
    }
    return $sequence;
}

sub nextSequence {
    my ($sequence) = @_;
    my $result = "";
    for (my $i = 0; $i < length($sequence);) {
        my $count = 1;
        my $digit = substr($sequence, $i, 1);
        for (my $j = $i + 1; $j < length($sequence) && substr($sequence, $j, 1) eq $digit; $j++) {
            $count++;
        }
        $result .= $count . $digit;
        $i += $count;
    }
    return $result;
}
