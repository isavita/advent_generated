
use strict;
use warnings;
use List::Util qw(min max);

my @bricks;

sub parseInput {
    my @input = @_;
    foreach my $line (@input) {
        my ($mini_x, $mini_y, $mini_z, $maxi_x, $maxi_y, $maxi_z) = split /,|~/, $line;
        push @bricks, {
            mini => { x => $mini_x, y => $mini_y, z => $mini_z },
            maxi => { x => $maxi_x, y => $maxi_y, z => $maxi_z },
            basedOn => [],
            support => []
        };
    }
}

sub settle {
    @bricks = sort { $a->{maxi}->{z} <=> $b->{maxi}->{z} } @bricks;

    foreach my $i (0..$#bricks) {
        my $brick = $bricks[$i];
        my $supportZ = 0;
        my @basedBricks;

        foreach my $j (reverse 0..$i-1) {
            my $isIntersectingX = max($brick->{mini}->{x}, $bricks[$j]->{mini}->{x}) <= min($brick->{maxi}->{x}, $bricks[$j]->{maxi}->{x});
            my $isIntersectingY = max($brick->{mini}->{y}, $bricks[$j]->{mini}->{y}) <= min($brick->{maxi}->{y}, $bricks[$j]->{maxi}->{y});
            my $isIntersecting = $isIntersectingX && $isIntersectingY;
            if ($isIntersecting) {
                if ($bricks[$j]->{maxi}->{z} == $supportZ) {
                    push @basedBricks, $bricks[$j];
                } elsif ($bricks[$j]->{maxi}->{z} > $supportZ) {
                    $supportZ = $bricks[$j]->{maxi}->{z};
                    @basedBricks = ($bricks[$j]);
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
    my $cnt = 0;
    foreach my $brick (@bricks) {
        my %fallingBricks;
        foreach my $supportedBrick (@{$brick->{support}}) {
            if (scalar @{$supportedBrick->{basedOn}} == 1) {
                my @allSupportedBricks = ($supportedBrick);
                while (@allSupportedBricks) {
                    my $supportedBrick0 = shift @allSupportedBricks;
                    my $isFalling = 1;
                    foreach my $basedBrick (@{$supportedBrick0->{basedOn}}) {
                        if (!exists $fallingBricks{$basedBrick} && $basedBrick != $brick) {
                            $isFalling = 0;
                            last;
                        }
                    }

                    if ($isFalling) {
                        $fallingBricks{$supportedBrick0} = 1;
                        push @allSupportedBricks, @{$supportedBrick0->{support}};
                    }
                }
            }
        }
        $cnt += scalar keys %fallingBricks;
    }
    return $cnt;
}

sub readFile {
    my ($fileName) = @_;
    open my $fh, '<', $fileName or die "Cannot open file: $!";
    my @lines = <$fh>;
    close $fh;
    chomp @lines;
    return @lines;
}

parseInput(readFile("input.txt"));
settle();
print solve() . "\n";
