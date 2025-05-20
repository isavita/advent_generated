
unit sub MAIN;

my $image-data = 'input.txt'.IO.slurp.chomp;
my $width = 25;
my $height = 6;
my $layer-size = $width * $height;

my @layers = $image-data.comb($layer-size);

my @final-image;
for 0 .. $layer-size - 1 -> $i {
    my $determining-pixel = @layers.map({ $_.substr($i, 1) }).first({ $_ ne '2' });
    @final-image[$i] = ($determining-pixel eq '1' ?? '#' !! ' ');
}

for @final-image.rotor($width) -> @row {
    say @row.join('');
}
