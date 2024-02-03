
use strict;
use warnings;

sub readInput {
    my $filename = shift;
    open(my $fh, '<', $filename) or die "Error reading input: $!";
    my $sequence = <$fh>;
    close($fh);
    return $sequence;
}

sub lookAndSay {
    my ($sequence, $iterations) = @_;
    for (my $i = 0; $i < $iterations; $i++) {
        $sequence = nextSequence($sequence);
    }
    return $sequence;
}

sub nextSequence {
    my $sequence = shift;
    my $result = '';
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

my $initialSequence = readInput("input.txt");
my $result = lookAndSay($initialSequence, 50);
print length($result) . "\n";
