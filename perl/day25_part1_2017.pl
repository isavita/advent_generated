use strict;
use warnings;
use File::Slurp;

sub parse_input {
    my ($file) = @_;
    my @lines = read_file($file, chomp => 1);
    my $initial_state = substr($lines[0], -2, 1);
    my ($steps) = $lines[1] =~ /(\d+)/;

    my %states;
    for (my $i = 3; $i < @lines; $i += 10) {
        my $state = substr($lines[$i], -2, 1);
        my $value0 = substr($lines[$i+2], -2, 1);
        my $move0 = $lines[$i+3] =~ /left/ ? -1 : 1;
        my $next_state0 = substr($lines[$i+4], -2, 1);
        my $value1 = substr($lines[$i+6], -2, 1);
        my $move1 = $lines[$i+7] =~ /left/ ? -1 : 1;
        my $next_state1 = substr($lines[$i+8], -2, 1);
        $states{$state} = {
            0 => [$value0, $move0, $next_state0],
            1 => [$value1, $move1, $next_state1],
        };
    }
    return ($initial_state, $steps, \%states);
}

sub run_turing_machine {
    my ($file) = @_;
    my ($state, $steps, $states) = parse_input($file);
    my %tape;
    my $cursor = 0;
    my $checksum = 0;

    for (1..$steps) {
        my $value = $tape{$cursor} // 0;
        my ($new_value, $move, $next_state) = @{$states->{$state}{$value}};
        $tape{$cursor} = $new_value;
        $cursor += $move;
        $state = $next_state;
    }

    $checksum += $_ for values %tape;
    return $checksum;
}

print run_turing_machine("input.txt"), "\n";