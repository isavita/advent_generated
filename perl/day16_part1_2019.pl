
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $input = <$fh>;
chomp $input;
close $fh;

my @digits = split //, $input;

for (1..100) {
    @digits = apply_fft(\@digits);
}

print join('', @digits[0..7]), "\n";

sub apply_fft {
    my ($input) = @_;
    my @base_pattern = (0, 1, 0, -1);
    my @output;

    for my $i (0 .. $#$input) {
        my $sum = 0;
        for my $j (0 .. $#$input) {
            my $pattern_value = $base_pattern[($j + 1) / ($i + 1) % @base_pattern];
            $sum += $input->[$j] * $pattern_value;
        }
        push @output, abs($sum) % 10;
    }
    return @output;
}

sub abs {
    return $_[0] < 0 ? -$_[0] : $_[0];
}
