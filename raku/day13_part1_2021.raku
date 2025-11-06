
sub MAIN {
    my @lines  = 'input.txt'.IO.lines;
    my $blank = @lines.first('', :k);

    my %dots = @lines[^$blank].map: { my ($x,$y)=.split(','); ($x,$y).join(',') => 1 }

    my ($axis,$val) = @lines[$blank+1].split(' ')[2].split('=');
    $val.=Int;

    my %folded;
    for %dots.keys -> $k {
        my ($x,$y) = $k.split(',');
        $x = 2*$val - $x if $axis eq 'x' && $x > $val;
        $y = 2*$val - $y if $axis eq 'y' && $y > $val;
        %folded{"$x,$y"} = 1;
    }

    say %folded.elems;
}
