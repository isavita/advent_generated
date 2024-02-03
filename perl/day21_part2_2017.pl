
use strict;
use warnings;

my %memo;
my %rules;

open(my $fh, "<", "input.txt") or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(" => ", $line);
    $rules{$parts[0]} = $parts[1];
}
close($fh);

my @grid = (
    ".#.",
    "..#",
    "###"
);

for (my $i = 0; $i < 18; $i++) {
    my $newSize;
    my $subSize;

    if (@grid % 2 == 0) {
        $subSize = 2;
        $newSize = @grid / 2 * 3;
    } else {
        $subSize = 3;
        $newSize = @grid / 3 * 4;
    }

    my @newGrid = ("") x $newSize;

    for (my $y = 0; $y < @grid; $y += $subSize) {
        for (my $x = 0; $x < @grid; $x += $subSize) {
            my @square;
            for (my $dy = 0; $dy < $subSize; $dy++) {
                push @square, substr($grid[$y + $dy], $x, $subSize);
            }
            my $newSquare = enhance(join("/", @square), \%rules);
            my @newRows = split("/", $newSquare);
            for my $dy (0..$#newRows) {
                $newGrid[$y / $subSize * ($subSize + 1) + $dy] .= $newRows[$dy];
            }
        }
    }
    @grid = @newGrid;
}

my $count = 0;
foreach my $row (@grid) {
    foreach my $pixel (split("", $row)) {
        $count++ if $pixel eq '#';
    }
}
print "$count\n";

sub enhance {
    my ($input, $rules) = @_;
    return $memo{$input} if exists $memo{$input};

    my $original = $input;
    for (my $i = 0; $i < 4; $i++) {
        if (exists $rules->{$input}) {
            $memo{$original} = $rules->{$input};
            return $rules->{$input};
        }
        $input = rotate($input);
    }
    $input = flip($input);
    for (my $i = 0; $i < 4; $i++) {
        if (exists $rules->{$input}) {
            $memo{$original} = $rules->{$input};
            return $rules->{$input};
        }
        $input = rotate($input);
    }
    return "";
}

sub rotate {
    my ($input) = @_;
    my @parts = split("/", $input);
    my $size = @parts;
    my @newParts;
    for my $x (0..$size-1) {
        my $newRow;
        for my $y (reverse 0..$size-1) {
            $newRow .= substr($parts[$y], $x, 1);
        }
        push @newParts, $newRow;
    }
    return join("/", @newParts);
}

sub flip {
    my ($input) = @_;
    my @parts = split("/", $input);
    foreach my $part (@parts) {
        $part = reverse($part);
    }
    return join("/", @parts);
}

sub reverse {
    my ($input) = @_;
    my @runes = split("", $input);
    for (my $i = 0, my $j = $#runes; $i < $j; $i++, $j--) {
        ($runes[$i], $runes[$j]) = ($runes[$j], $runes[$i]);
    }
    return join("", @runes);
}
