
sub MAIN() {
    my $line = "input.txt".IO.slurp.trim;
    $line ~~ /row \s+ (\d+) \s* ',' \s* column \s+ (\d+)/;
    my ($row, $col) = $0, $1;
    
    my $pos = ($row + $col - 2) * ($row + $col - 1) div 2 + $col;
    
    my $code = 20151125;
    $code = ($code * 252533) % 33554393 for 1..^$pos;
    
    say $code;
}
