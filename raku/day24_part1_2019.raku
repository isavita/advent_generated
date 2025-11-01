
sub MAIN {
    my @grid = 'input.txt'.IO.lines;
    my %seen;

    loop {
        my $layout = @grid.join;
        if %seen{$layout}:exists { say [+] (^25).map({ 2**$_ if $layout.substr($_,1) eq '#' }); exit }
        %seen{$layout} = 1;

        my @next;
        for ^5 -> $i {
            my $row = '';
            for ^5 -> $j {
                my $bugs = [+] flat
                    ($i-1..$i+1).map: -> $x {
                        ($j-1..$j+1).map: -> $y {
                            next unless abs($x-$i) + abs($y-$j) == 1;
                            ($x == any(0..4) and $y == any(0..4) and @grid[$x].substr($y,1) eq '#') ?? 1 !! 0
                        }
                    }
                $row ~= @grid[$i].substr($j,1) eq '#' ?? ($bugs == 1 ?? '#' !! '.') !! ($bugs == 1|2 ?? '#' !! '.');
            }
            @next.push: $row;
        }
        @grid = @next;
    }
}
