
sub MAIN {
    my ($twos, $threes) = (0, 0);
    for 'input.txt'.IO.lines -> $box-id {
        my %counts = $box-id.comb.classify(*);
        if %counts.values.any == 2 {
            $twos++;
        }
        if %counts.values.any == 3 {
            $threes++;
        }
    }
    ($twos * $threes).say;
}
