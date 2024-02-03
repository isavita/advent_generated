
use strict;
use warnings;

sub readInput {
    my $filename = shift;
    open my $fh, '<', $filename or die $!;
    my $algorithm = <$fh>;
    chomp $algorithm;
    <$fh>; # Skip the empty line

    my @image;
    while (my $line = <$fh>) {
        chomp $line;
        push @image, [split //, $line];
    }

    close $fh;
    return $algorithm, \@image;
}

sub enhanceImage {
    my ($image, $algorithm, $times) = @_;
    for (my $i = 0; $i < $times; $i++) {
        $image = applyAlgorithm($image, $algorithm, $i % 2 == 1 && substr($algorithm, 0, 1) eq '#');
    }
    return $image;
}

sub applyAlgorithm {
    my ($image, $algorithm, $flip) = @_;
    my @enhancedImage;
    for my $i (0..$#{$image}+2) {
        for my $j (0..$#{$image->[0]}+2) {
            my $index = calculateIndex($i-1, $j-1, $image, $flip);
            $enhancedImage[$i][$j] = substr($algorithm, $index, 1);
        }
    }
    return \@enhancedImage;
}

sub calculateIndex {
    my ($i, $j, $image, $flip) = @_;
    my $index = 0;
    for my $di (-1..1) {
        for my $dj (-1..1) {
            $index <<= 1;
            if ($i+$di >= 0 && $i+$di < @{$image} && $j+$dj >= 0 && $j+$dj < @{$image->[0]}) {
                if ($image->[$i+$di][$j+$dj] eq '#') {
                    $index |= 1;
                }
            } elsif ($flip) {
                $index |= 1;
            }
        }
    }
    return $index;
}

sub countLitPixels {
    my $image = shift;
    my $count = 0;
    for my $row (@{$image}) {
        for my $pixel (@{$row}) {
            $count++ if $pixel eq '#';
        }
    }
    return $count;
}

my ($algorithm, $image) = readInput("input.txt");
$image = enhanceImage($image, $algorithm, 2);
print countLitPixels($image) . "\n";
