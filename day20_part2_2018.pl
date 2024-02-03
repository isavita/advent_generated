
use strict;
use warnings;

my $filename = 'input.txt';
open(my $fh, '<:encoding(UTF-8)', $filename)
  or die "Could not open file '$filename' $!";

my $regex = <$fh>;
close($fh);

my %dm = buildMap(substr($regex, 1, length($regex) - 2));
my $rooms = countRooms(\%dm, 1000);
print "$rooms\n";

sub buildMap {
    my $regex = shift;
    my %dm;
    my @stack;
    my @cp = (0, 0);
    foreach my $c (split //, $regex) {
        if ($c eq '(') {
            push @stack, [@cp];
        } elsif ($c eq '|') {
            @cp = @{$stack[$#stack]};
        } elsif ($c eq ')') {
            @cp = @{$stack[$#stack]};
            pop @stack;
        } else {
            my @np = move(\@cp, $c);
            $dm{"@cp"}{"@np"} = 1;
            @cp = @np;
        }
    }
    return %dm;
}

sub move {
    my ($p, $dir) = @_;
    if ($dir eq 'N') {
        return ($p->[0], $p->[1] - 1);
    } elsif ($dir eq 'S') {
        return ($p->[0], $p->[1] + 1);
    } elsif ($dir eq 'E') {
        return ($p->[0] + 1, $p->[1]);
    } elsif ($dir eq 'W') {
        return ($p->[0] - 1, $p->[1]);
    }
    return @$p;
}

sub countRooms {
    my ($dm, $minDoors) = @_;
    my %visited;
    my @queue = ([0, 0]);
    my $roomCount = 0;

    while (@queue) {
        my $p = shift @queue;
        foreach my $np (keys %{$dm->{"@$p"}}) {
            if (!exists $visited{$np}) {
                $visited{$np} = $visited{"@$p"} + 1;
                if ($visited{$np} >= $minDoors) {
                    $roomCount++;
                }
                push @queue, [split ' ', $np];
            }
        }
    }
    return $roomCount;
}
