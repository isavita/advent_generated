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

my @jetPattern = split //, readAll("input.txt");

my @rocks = getRocks();
my %grid;
for my $x (0..6) {
    $grid{"$x,0"} = 1;
}
my ($floor, $j) = (0, 0);
my %repeat;

for (my ($i, $curr) = (0, 0); ; ($i, $curr) = ($i+1, ($curr+1)%@rocks)) {
    my $key = "$curr,$j";
    if (exists $repeat{$key}) {
        my ($previ, $prevFloor) = @{$repeat{$key}};
        if ((1000000000000-$i) % ($i-$previ) == 0) {
            print $floor + (1000000000000-$i)/($i-$previ)*($floor-$prevFloor), "\n";
            last;
        }
    }
    $repeat{$key} = [$i, $floor];
    my $currRock = $rocks[$curr];
    my ($pos_x, $pos_y) = (2, $floor+4);
    while (1) {
        my $jet = $jetPattern[$j];
        $j = ($j + 1) % @jetPattern;
        my ($dx, $dy) = dirFromByte($jet);
        ($pos_x, $pos_y) = ($pos_x+$dx, $pos_y+$dy);
        if (collision(\%grid, $currRock, $pos_x, $pos_y)) {
            ($pos_x, $pos_y) = ($pos_x-$dx, $pos_y-$dy);
        }
        ($pos_x, $pos_y) = ($pos_x, $pos_y-1);
        if (collision(\%grid, $currRock, $pos_x, $pos_y)) {
            ($pos_x, $pos_y) = ($pos_x, $pos_y+1);
            for my $p (keys %$currRock) {
                my ($x, $y) = split /,/, $p;
                $grid{($pos_x+$x).",".($pos_y+$y)} = 1;
                $floor = max($floor, $pos_y+$y);
            }
            last;
        }
    }
}

sub collision {
    my ($grid, $rock, $pos_x, $pos_y) = @_;
    for my $p (keys %$rock) {
        my ($x, $y) = split /,/, $p;
        return 1 if exists $grid->{($pos_x+$x).",".($pos_y+$y)} || $pos_x+$x < 0 || $pos_x+$x > 6;
    }
    return 0;
}

sub getRocks {
    my @rocks;
    for my $rock (split /\n\n/, $rockstr) {
        my %rock;
        my @lines = split /\n/, $rock;
        for my $y (0..$#lines) {
            my $line = $lines[$y];
            for my $x (0..length($line)-1) {
                $rock{$x.",".(@lines-1-$y)} = 1 if substr($line, $x, 1) eq '#';
            }
        }
        push @rocks, \%rock;
    }
    return @rocks;
}

sub readAll {
    my ($path) = @_;
    open my $fh, '<', $path or die "Could not open file '$path': $!";
    my $content = do { local $/; <$fh> };
    close $fh;
    return $content =~ s/\s+$//r;
}

sub dirFromByte {
    my ($b) = @_;
    return (1, 0) if $b eq '>'; # E
    return (-1, 0) if $b eq '<'; # W
}