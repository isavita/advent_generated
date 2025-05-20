
sub MAIN {
    my $total_fuel = sum 'input.txt'.IO.lines.map({ $_.Int div 3 - 2 });
    say $total_fuel;
}
