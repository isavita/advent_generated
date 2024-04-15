use strict;
use warnings;
use List::Util qw(max);

my $rockstr = "####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##";

my @rocks = getRocks();
my %grid;
for my $x (0..6) {
    $grid{"$x,0"} = 1;
}
my ($floor, $j) = (0, 0);
my %repeat;

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my $jetPattern = do { local $/; <$fh> };
chomp $jetPattern;
close $fh;

my ($i, $curr) = (0, 0);
while (1) {
    if ($i == 2022) {
        print "$floor\n";
        last;
    }
    my $key = "$curr,$j";
    $repeat{$key} = "$i,$floor";
    my $currRock = $rocks[$curr];
    my ($pos_x, $pos_y) = (2, $floor + 4);
    while (1) {
        my $jet = substr($jetPattern, $j, 1);
        $j = ($j + 1) % length($jetPattern);
        my ($dx, $dy) = dirFromByte($jet);
        ($pos_x, $pos_y) = ($pos_x + $dx, $pos_y + $dy);
        if (collision(\%grid, $currRock, $pos_x, $pos_y)) {
            ($pos_x, $pos_y) = ($pos_x - $dx, $pos_y - $dy);
        }
        ($pos_x, $pos_y) = ($pos_x, $pos_y - 1);
        if (collision(\%grid, $currRock, $pos_x, $pos_y)) {
            ($pos_x, $pos_y) = ($pos_x, $pos_y + 1);
            foreach my $p (keys %$currRock) {
                my ($x, $y) = split /,/, $p;
                $grid{($pos_x + $x) . "," . ($pos_y + $y)} = 1;
                $floor = max($floor, $pos_y + $y);
            }
            last;
        }
    }
    $i++;
    $curr = ($curr + 1) % @rocks;
}

sub collision {
    my ($grid, $rock, $pos_x, $pos_y) = @_;
    foreach my $p (keys %$rock) {
        my ($x, $y) = split /,/, $p;
        return 1 if exists $grid->{($pos_x + $x) . "," . ($pos_y + $y)};
        return 1 if $pos_x + $x < 0 || $pos_x + $x > 6;
    }
    return 0;
}

sub getRocks {
    my @rocks;
    foreach my $rock (split /\n\n/, $rockstr) {
        my %rock;
        my @lines = split /\n/, $rock;
        for my $y (0..$#lines) {
            my $line = $lines[$y];
            for my $x (0..length($line)-1) {
                if (substr($line, $x, 1) eq '#') {
                    $rock{$x . "," . ($#lines - $y)} = 1;
                }
            }
        }
        push @rocks, \%rock;
    }
    return @rocks;
}

sub dirFromByte {
    my $b = shift;
    return (1, 0) if $b eq '>';
    return (-1, 0) if $b eq '<';
    return (0, 0);
}