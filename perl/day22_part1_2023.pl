
use strict;
use warnings;
use List::Util qw(max min);

my @bricks;

sub parseInput {
    my @input = @_;
    my @parsed_bricks;
    foreach my $line (@input) {
        my @coords = split('~', $line);
        my @mini_coords = split(',', $coords[0]);
        my @maxi_coords = split(',', $coords[1]);
        my %brick = (
            mini => {
                x => $mini_coords[0],
                y => $mini_coords[1],
                z => $mini_coords[2]
            },
            maxi => {
                x => $maxi_coords[0],
                y => $maxi_coords[1],
                z => $maxi_coords[2]
            },
            basedOn => [],
            support => []
        );
        push @parsed_bricks, \%brick;
    }
    return \@parsed_bricks;
}

sub settle {
    my @sorted_bricks = sort { $a->{maxi}->{z} <=> $b->{maxi}->{z} } @_;
    for (my $i = 0; $i < scalar @sorted_bricks; $i++) {
        my $brick = $sorted_bricks[$i];
        my $supportZ = 0;
        my @basedBricks;
        for (my $j = $i - 1; $j > -1; $j--) {
            my $isIntersectingX = max($brick->{mini}->{x}, $sorted_bricks[$j]->{mini}->{x}) <= min($brick->{maxi}->{x}, $sorted_bricks[$j]->{maxi}->{x});
            my $isIntersectingY = max($brick->{mini}->{y}, $sorted_bricks[$j]->{mini}->{y}) <= min($brick->{maxi}->{y}, $sorted_bricks[$j]->{maxi}->{y});
            my $isIntersecting = $isIntersectingX && $isIntersectingY;
            if ($isIntersecting) {
                if ($sorted_bricks[$j]->{maxi}->{z} == $supportZ) {
                    push @basedBricks, $sorted_bricks[$j];
                } elsif ($sorted_bricks[$j]->{maxi}->{z} > $supportZ) {
                    $supportZ = $sorted_bricks[$j]->{maxi}->{z};
                    @basedBricks = ($sorted_bricks[$j]);
                }
            }
        }
        $brick->{basedOn} = \@basedBricks;
        foreach my $basedBrick (@basedBricks) {
            push @{$basedBrick->{support}}, $brick;
        }
        my $deltaZ = $brick->{maxi}->{z} - $brick->{mini}->{z};
        $brick->{mini}->{z} = $supportZ + 1;
        $brick->{maxi}->{z} = $brick->{mini}->{z} + $deltaZ;
    }
}

sub solve {
    my @input = @_;
    my $bricks_ref = parseInput(@input);
    @bricks = @{$bricks_ref};
    settle(@bricks);

    my $cnt = 0;
    foreach my $brick (@bricks) {
        my $isDisintegratable = 1;
        foreach my $supportedBrick (@{$brick->{support}}) {
            if (scalar @{$supportedBrick->{basedOn}} < 2) {
                $isDisintegratable = 0;
                last;
            }
        }
        if ($isDisintegratable) {
            $cnt++;
        }
    }
    return $cnt;
}

sub readFile {
    my $fileName = shift;
    open(my $fh, '<', $fileName) or die "Cannot open file: $!";
    my @lines = <$fh>;
    close($fh);
    chomp @lines;
    return @lines;
}

my @input = readFile("input.txt");
print solve(@input) . "\n";
