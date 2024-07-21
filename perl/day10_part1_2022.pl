
use strict;
use warnings;

# Read input from the file
open my $fh, '<', 'input.txt' or die "Could not open file: $!";
my @instructions = <$fh>;
chomp @instructions;
close $fh;

# Initialize variables
my $X = 1;
my $cycle = 0;
my %signal_strengths;

# Define the cycles we are interested in
my @target_cycles = (20, 60, 100, 140, 180, 220);

# Process each instruction
foreach my $instruction (@instructions) {
    if ($instruction eq 'noop') {
        # No operation takes 1 cycle
        $cycle++;
        record_signal_strength($cycle, $X, \%signal_strengths, \@target_cycles);
    } else {
        # addx V takes 2 cycles
        my ($cmd, $value) = split ' ', $instruction;
        my $v = int($value);
        
        # First cycle of addx
        $cycle++;
        record_signal_strength($cycle, $X, \%signal_strengths, \@target_cycles);
        
        # Second cycle of addx
        $cycle++;
        record_signal_strength($cycle, $X, \%signal_strengths, \@target_cycles);
        
        # Update X after the second cycle
        $X += $v;
    }
}

# Calculate the sum of the signal strengths
my $sum_signal_strengths = 0;
$sum_signal_strengths += $_ for values %signal_strengths;

# Print the result
print "The sum of the signal strengths is: $sum_signal_strengths\n";

# Subroutine to record signal strength at target cycles
sub record_signal_strength {
    my ($cycle, $X, $signal_strengths_ref, $target_cycles_ref) = @_;
    if (grep { $_ == $cycle } @$target_cycles_ref) {
        $signal_strengths_ref->{$cycle} = $cycle * $X;
    }
}
