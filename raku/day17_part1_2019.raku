
sub MAIN {
    my @program = 'input.txt'.IO.slurp.trim.split(',').map: *.Int;
    my @out = run(@program, []);
    my $map = @out.map(*.chr).join;
    my %grid;
    my ($rx,$ry);
    for $map.lines.kv -> $y,$line {
        for $line.comb.kv -> $x,$c {
            if $c eq '#' { %grid{"$x,$y"} = 1 }
            elsif $c ~~ /<[\^v<>]>/ { ($rx,$ry) = ($x,$y) }
        }
    }
    my $sum = 0;
    for %grid.kv -> $k,$_ {
        my ($x,$y) = $k.split(',').map: *.Int;
        $sum += $x*$y
            if (1,0,-1,0,1).rotor(2=>-1).map(-> ($dx,$dy) { %grid{"{+$x+$dx},{+$y+$dy}"} }).all.so;
    }
    say $sum
}

sub run(@prog,@in) {
    my %m; %m{^@prog} = @prog;
    my ($ip,$rb,@out) = 0,0;
    sub mode($i,$mo) {
        given $mo {
            when 0 { %m{%m{$i} // 0} // 0 }
            when 1 { %m{$i} // 0 }
            when 2 { %m{%m{$i}+$rb} // 0 }
        }
    }
    sub set($i,$mo,$v) {
        given $mo {
            when 0 { %m{%m{$i}} = $v }
            when 2 { %m{%m{$i}+$rb} = $v }
        }
    }
    loop {
        my $n = %m{$ip};
        my $op = $n % 100;
        my @mo = ($n/100).Int.polymod(10,10,10);
        given $op {
            when 1  { set($ip+3,@mo[2],mode($ip+1,@mo[0]) + mode($ip+2,@mo[1])); $ip +=4 }
            when 2  { set($ip+3,@mo[2],mode($ip+1,@mo[0]) * mode($ip+2,@mo[1])); $ip +=4 }
            when 3  { set($ip+1,@mo[0],@in.shift); $ip +=2 }
            when 4  { @out.push: mode($ip+1,@mo[0]); $ip +=2 }
            when 5  { $ip = mode($ip+1,@mo[0]) ?? mode($ip+2,@mo[1]) !! $ip+3 }
            when 6  { $ip = mode($ip+1,@mo[0]) ?? $ip+3 !! mode($ip+2,@mo[1]) }
            when 7  { set($ip+3,@mo[2],mode($ip+1,@mo[0]) < mode($ip+2,@mo[1]) ?? 1 !! 0); $ip +=4 }
            when 8  { set($ip+3,@mo[2],mode($ip+1,@mo[0]) == mode($ip+2,@mo[1]) ?? 1 !! 0); $ip +=4 }
            when 9  { $rb += mode($ip+1,@mo[0]); $ip +=2 }
            when 99 { return @out }
            default { die "bad op $op" }
        }
    }
}
