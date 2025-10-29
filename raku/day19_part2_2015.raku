sub MAIN() {
    my $content = slurp 'input.txt';
    my @lines = $content.split("\n");
    my $molecule = '';
    for ^@lines -> $i {
        if @lines[$i] ~~ /^\s*$/ {
            $molecule = @lines[$i+1] // '';
            last;
        }
    }
    $molecule ~~ s/ \r //;
    my $total = 0;
    my $rn = 0;
    my $ar = 0;
    my $y = 0;
    my $i = 0;
    while $i < $molecule.chars {
        $total++;
        my $elem = $molecule.substr($i,1);
        if $i+1 < $molecule.chars && $molecule.substr($i+1,1) ~~ /<:Ll>/ {
            $elem ~= $molecule.substr($i+1,1);
            $i += 2;
        } else {
            $i += 1;
        }
        given $elem {
            when 'Rn' { $rn++ }
            when 'Ar' { $ar++ }
            when 'Y'  { $y++ }
        }
    }
    my $steps = $total - $rn - $ar - 2*$y - 1;
    say $steps;
}