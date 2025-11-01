
sub MAIN() {
    my %grid;
    my ($start-x, $start-y);
    my $current-y = 0;

    for 'input.txt'.IO.lines {
        my $current-x = 0;
        for .comb {
            %grid{"$current-x,$current-y"} = True if $_ eq '#';
            $current-x++;
        }
        $start-x = ($current-x - 1) div 2 if $current-y == 0;
        $current-y++;
    }
    $start-y = ($current-y - 1) div 2;

    my @dx = 0, 1, 0, -1;
    my @dy = -1, 0, 1, 0;

    my ($x, $y) = $start-x, $start-y;
    my $dir = 0;
    my $infected = 0;

    for ^10000 {
        my $key = "$x,$y";
        if %grid{$key}:exists {
            $dir = ($dir + 1) % 4;
            %grid{$key}:delete;
        } else {
            $dir = ($dir - 1 + 4) % 4;
            %grid{$key} = True;
            $infected++;
        }
        $x += @dx[$dir];
        $y += @dy[$dir];
    }

    say $infected;
}
