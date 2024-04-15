use strict;
use warnings;

my $total_sum = 0;

open my $file, '<', 'input.txt' or die "Error opening file: $!";

while (my $line = <$file>) {
    if ($line =~ /Game (\d+): (.+)/) {
        my $game_id = $1;
        my @rounds = split /;/, $2;
        my $is_valid = 1;

        foreach my $round (@rounds) {
            my %count = (red => 0, green => 0, blue => 0);

            while ($round =~ /(\d+) (red|green|blue)/g) {
                my ($count, $color) = ($1, $2);
                $count{$color} += $count;

                if ($count{red} > 12 || $count{green} > 13 || $count{blue} > 14) {
                    $is_valid = 0;
                    last;
                }
            }

            last unless $is_valid;
        }

        $total_sum += $game_id if $is_valid;
    }
}

close $file;

print "$total_sum\n";