
use strict;
use warnings;

sub parse_input {
    my @histories = ();
    foreach my $line (@_) {
        my @numbers = parse_string_to_ints($line);
        push @histories, \@numbers;
    }
    return @histories;
}

sub parse_string_to_ints {
    my @numbers = ();
    my @number_parts = split(' ', $_[0]);
    foreach my $number_str (@number_parts) {
        my $number = int($number_str);
        push @numbers, $number;
    }
    return @numbers;
}

sub all_zeros {
    my @nums = @{$_[0]};
    foreach my $num (@nums) {
        if ($num != 0) {
            return 0;
        }
    }
    return 1;
}

sub calculate_extrapolation {
    my @history = @{$_[0]};
    my @extrapolations = ();
    for (my $i = 1; $i < scalar(@history); $i++) {
        my $extrapolation = $history[$i] - $history[$i-1];
        push @extrapolations, $extrapolation;
    }
    return @extrapolations;
}

sub calculate_extrapolations {
    my @history = @{$_[0]};
    my @extrapolations_series = ();
    push @extrapolations_series, \@history;

    for (my $i = 1; $i < scalar(@history); $i++) {
        my @previous_extrapolations = @{$extrapolations_series[$i-1]};
        if (all_zeros(\@previous_extrapolations)) {
            return @extrapolations_series;
        }

        my @extrapolations = calculate_extrapolation(\@previous_extrapolations);
        push @extrapolations_series, \@extrapolations;
    }

    return @extrapolations_series;
}

sub solve {
    my @input = @_;
    my $res = 0;

    my @histories = parse_input(@input);

    foreach my $history (@histories) {
        my @extrapolations_series = calculate_extrapolations($history);

        my $past_prediction = 0;
        for (my $i = scalar(@extrapolations_series) - 1; $i > -1; $i--) {
            $past_prediction = $extrapolations_series[$i][0] - $past_prediction;
        }

        $res += $past_prediction;
    }

    return $res;
}

sub read_file {
    open(my $fh, '<', "input.txt") or die "Cannot open file: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = read_file();
print solve(@input) . "\n";
