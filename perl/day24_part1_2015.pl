
use strict;
use warnings;
use List::Util qw(sum);

my @packages = do {
    open my $fh, '<', 'input.txt' or die $!;
    <$fh>;
};
chomp @packages;
my $total_weight = sum @packages;
my $target_weight = $total_weight / 3;
my ($best_qe, $best_length) = (1e18, 1e18);

sub find_combinations {
    my ($index, $current_weight, $current_qe, $current_length) = @_;
    return if $current_weight > $target_weight;
    if ($current_weight == $target_weight) {
        if ($current_length < $best_length || ($current_length == $best_length && $current_qe < $best_qe)) {
            ($best_length, $best_qe) = ($current_length, $current_qe);
        }
    }
    for my $i ($index .. $#packages) {
        find_combinations($i + 1, $current_weight + $packages[$i], $current_qe * $packages[$i], $current_length + 1);
    }
}

find_combinations(0, 0, 1, 0);
print "$best_qe\n";
