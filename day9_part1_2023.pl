
use strict;
use warnings;

sub parseInput {
    my @input = @_;
    my @histories = ();
    foreach my $line (@input) {
        my @numbers = parseStringToInts($line);
        push @histories, \@numbers;
    }
    return @histories;
}

sub parseStringToInts {
    my $numbersLine = shift;
    my @numbers = ();
    my @numbersParts = split(' ', $numbersLine);
    foreach my $numberStr (@numbersParts) {
        my $number = int($numberStr);
        push @numbers, $number;
    }
    return @numbers;
}

sub allZeros {
    my @nums = @_;
    foreach my $num (@nums) {
        if ($num != 0) {
            return 0;
        }
    }
    return 1;
}

sub calculateExtrapolation {
    my @history = @_;
    my @extrapolations = ();
    for (my $i = 1; $i < scalar(@history); $i++) {
        my $extrapolation = $history[$i] - $history[$i-1];
        push @extrapolations, $extrapolation;
    }
    return @extrapolations;
}

sub calculateExtrapolations {
    my @history = @_;
    my @extrapolationsSeries = ();
    push @extrapolationsSeries, \@history;

    for (my $i = 1; $i < scalar(@history); $i++) {
        my @previousExtrapolations = @{$extrapolationsSeries[$i-1]};
        if (allZeros(@previousExtrapolations)) {
            return @extrapolationsSeries;
        }

        my @extrapolations = calculateExtrapolation(@previousExtrapolations);
        push @extrapolationsSeries, \@extrapolations;
    }

    return @extrapolationsSeries;
}

sub solve {
    my @input = @_;
    my @histories = parseInput(@input);
    my $res = 0;

    foreach my $history (@histories) {
        my @extrapolationsSeries = calculateExtrapolations(@$history);

        my $futurePrediction = 0;
        for (my $i = scalar(@extrapolationsSeries) - 1; $i > -1; $i--) {
            $futurePrediction = $extrapolationsSeries[$i][scalar(@{$extrapolationsSeries[$i]})-1] + $futurePrediction;
        }

        $res += $futurePrediction;
    }

    return $res;
}

sub readFile {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file $fileName: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input) . "\n";
