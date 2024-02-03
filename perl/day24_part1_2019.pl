
use strict;
use warnings;

my $Side = 5;
my $Square = $Side * $Side;

sub parse {
    my @res = ();

    open(my $fh, '<', 'input.txt') or die $!;
    my $row = 0;
    while (my $line = <$fh>) {
        chomp $line;
        my @chars = split('', $line);
        for my $col (0..$Side-1) {
            if ($chars[$col] eq '#') {
                $res[$row*$Side+$col] = 1;
            } else {
                $res[$row*$Side+$col] = 0;
            }
        }
        $row++;
    }
    close($fh);
    return @res;
}

my %appeared = ();

my @grid = parse();
$appeared{join(',', @grid)} = 1;
while (1) {
    @grid = next1(@grid);
    if ($appeared{join(',', @grid)}) {
        print biodiversity(@grid) . "\n";
        exit;
    }
    $appeared{join(',', @grid)} = 1;
}

sub next1 {
    my @grid = @_;
    my @newGrid = ();

    for my $i (0..$Square-1) {
        my $row = int($i/$Side);
        my $col = $i%$Side;
        my $neighbours = 0;

        if ($row > 0) {
            $neighbours++ if $grid[$i-$Side];
        }
        if ($row < $Side-1) {
            $neighbours++ if $grid[$i+$Side];
        }
        if ($col > 0) {
            $neighbours++ if $grid[$i-1];
        }
        if ($col < $Side-1) {
            $neighbours++ if $grid[$i+1];
        }

        if ($grid[$i] && $neighbours != 1) {
            $newGrid[$i] = 0;
            next;
        }

        if (!$grid[$i] && ($neighbours == 1 || $neighbours == 2)) {
            $newGrid[$i] = 1;
            next;
        }

        $newGrid[$i] = $grid[$i];
    }

    return @newGrid;
}

sub biodiversity {
    my @grid = @_;
    my $bio = 0;
    for my $i (0..$Square-1) {
        $bio += 1 << $i if $grid[$i];
    }
    return $bio;
}
