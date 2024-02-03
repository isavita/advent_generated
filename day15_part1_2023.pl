
use strict;
use warnings;

my $hashTableSize = 256;

sub hashString {
    my $str = shift;
    my $res = 0;
    for (my $i = 0; $i < length($str); $i++) {
        my $char = substr($str, $i, 1);
        $res += ord($char);
        $res *= 17;
        $res %= $hashTableSize;
    }
    return $res;
}

sub parseStep {
    my $stepStr = shift;
    my %step;

    $step{Label} = $stepStr =~ s/[-=0123456789]//gr;
    $step{NumBox} = hashString($step{Label});
    $step{Operation} = substr($stepStr, length($step{Label}), 1);
    if ($step{Operation} eq "=") {
        my $number = substr($stepStr, length($step{Label}) + 1);
        $step{Number} = $number;
    }

    return \%step;
}

sub solve {
    my @input = @_;
    my $line = $input[0];
    my @steps = split(",", $line);
    my $res = 0;
    foreach my $step (@steps) {
        $res += hashString($step);
    }
    return $res;
}

sub readFile {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file $fileName: $!";
    my @lines = <$fh>;
    close($fh);
    chomp(@lines);
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input);
