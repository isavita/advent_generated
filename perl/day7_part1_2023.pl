
use strict;
use warnings;

my @matches = ([], [], [], [], [], [], []);

sub findMatches {
    my @hands = @_;

    foreach my $hand (@hands) {
        my %count;
        my @cards = split('', $hand->{cards});

        foreach my $card (@cards) {
            $count{$card}++;
        }

        my $value = 1;
        foreach my $c (values %count) {
            $value *= $c;
        }

        if ($value == 1) {
            push @{$matches[6]}, $hand;
        } elsif ($value == 2) {
            push @{$matches[5]}, $hand;
        } elsif ($value == 3) {
            push @{$matches[3]}, $hand;
        } elsif ($value == 4) {
            if (scalar keys %count == 2) {
                push @{$matches[1]}, $hand;
            } else {
                push @{$matches[4]}, $hand;
            }
        } elsif ($value == 5) {
            push @{$matches[0]}, $hand;
        } elsif ($value == 6) {
            push @{$matches[2]}, $hand;
        } else {
            print "oh no\n";
        }
    }
}

sub convertAndOrderMatches {
    my @convertedMatches;

    foreach my $category (@matches) {
        my @temp;

        foreach my $hand (@{$category}) {
            my $cards = $hand->{cards};
            $cards =~ s/A/E/g;
            $cards =~ s/T/A/g;
            $cards =~ s/J/B/g;
            $cards =~ s/Q/C/g;
            $cards =~ s/K/D/g;

            my $num = hex($cards);
            push @temp, {hand => $hand, rank => $num};
        }

        my @sorted = sort { $b->{rank} <=> $a->{rank} } @temp;
        push @convertedMatches, @sorted;
    }

    return @convertedMatches;
}

open my $fh, '<', 'input.txt' or die $!;
my @hands;

while (my $line = <$fh>) {
    chomp $line;
    next if $line eq '';

    my ($cards) = $line =~ /([\dAKQJT]+)/;
    my ($bid) = $line =~ / (\d+)/;

    push @hands, {cards => $cards, bid => $bid};
}

findMatches(@hands);
my @convertedMatches = convertAndOrderMatches();

my $total = 0;
for my $i (0..$#convertedMatches) {
    $total += $convertedMatches[$i]{hand}{bid} * ($#convertedMatches - $i + 1);
}

print "$total\n";
