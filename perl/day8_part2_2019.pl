
use strict;
use warnings;

my $width = 25;
my $height = 6;
my $layer_size = $width * $height;
my $final_image = '2' x $layer_size;

open my $fh, '<', 'input.txt' or die $!;
my $image_data = do { local $/; <$fh> };
close $fh;

$image_data =~ s/\s//g;

for (my $i = 0; $i < length($image_data); $i += $layer_size) {
    my $layer = substr($image_data, $i, $layer_size);
    for (my $j = 0; $j < length($layer); $j++) {
        my $pixel = substr($layer, $j, 1);
        substr($final_image, $j, 1) = $pixel if substr($final_image, $j, 1) eq '2';
    }
}

print "Decoded image:\n";
for (my $i = 0; $i < $height; $i++) {
    for (my $j = 0; $j < $width; $j++) {
        my $pixel = substr($final_image, $i * $width + $j, 1);
        print $pixel eq '0' ? ' ' : '#';
    }
    print "\n";
}
