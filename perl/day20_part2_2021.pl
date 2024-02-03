
use strict;
use warnings;

my $ITERATIONS = 50;
my $EXPAND_BY = 1;

my ($algorithm, @image) = read_input("input.txt");
for (my $i = 0; $i < $ITERATIONS; $i++) {
    @image = enhance_image($algorithm, \@image, $i % 2 == 1 && substr($algorithm, 0, 1) eq '#');
}
print count_lit_pixels(\@image) . "\n";

sub read_input {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Cannot open file: $!";
    my $algorithm = <$fh>;
    chomp $algorithm;

    <$fh>; # skip the empty line

    my @image;
    while (my $line = <$fh>) {
        chomp $line;
        my @row = map { $_ eq '#' } split('', $line);
        push @image, \@row;
    }
    close $fh;
    return ($algorithm, @image);
}

sub enhance_image {
    my ($algorithm, $image, $use_infinite_lit) = @_;
    my @new_image;
    for my $y (-$EXPAND_BY .. $#{$image} + $EXPAND_BY) {
        for my $x (-$EXPAND_BY .. $#{$image->[0]} + $EXPAND_BY) {
            my $index = 0;
            for my $dy (-1 .. 1) {
                for my $dx (-1 .. 1) {
                    $index <<= 1;
                    my $ny = $y + $dy;
                    my $nx = $x + $dx;
                    if ($ny >= 0 && $ny < @{$image} && $nx >= 0 && $nx < @{$image->[0]}) {
                        $index |= 1 if $image->[$ny][$nx];
                    } elsif ($use_infinite_lit) {
                        $index |= 1;
                    }
                }
            }
            $new_image[$y + $EXPAND_BY][$x + $EXPAND_BY] = substr($algorithm, $index, 1) eq '#';
        }
    }
    return @new_image;
}

sub count_lit_pixels {
    my ($image) = @_;
    my $count = 0;
    for my $row (@{$image}) {
        for my $pixel (@{$row}) {
            $count++ if $pixel;
        }
    }
    return $count;
}
