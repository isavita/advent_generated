
sub calculate-fuel(Int $mass) {
    my $fuel = $mass div 3 - 2;
    return 0 if $fuel <= 0;
    $fuel + calculate-fuel($fuel);
}

sub main() {
    my @modules = 'input.txt'.IO.lines.map(*.Int);
    my $total-fuel = @modules.map(&calculate-fuel).sum;
    say $total-fuel;
}

main();
