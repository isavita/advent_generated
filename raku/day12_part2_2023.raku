
use v6.d;

sub count-arrangements(Str $springs, @groups) {
    my %cache;
    sub inner(Int $s, Int $g, Int $run) {
        my $key = "$s,$g,$run";
        return %cache{$key} if %cache{$key}:exists;
        my $res = 0;
        if $s == $springs.chars {
            $res = 1 if $g == @groups && $run == 0;
            $res = 1 if $g == @groups - 1 && $run == @groups[*-1];
        } else {
            my $c = $springs.substr($s, 1);
            if $c eq '.' | '?' {
                $res += inner($s + 1, $g, 0) if $run == 0;
                $res += inner($s + 1, $g + 1, 0) if $g < @groups && $run == @groups[$g];
            }
            if $c eq '#' | '?' {
                $res += inner($s + 1, $g, $run + 1) if $g < @groups && $run < @groups[$g];
            }
        }
        %cache{$key} = $res;
    }
    inner(0, 0, 0);
}

sub MAIN {
    my $total = 0;
    for 'input.txt'.IO.lines {
        my ($springs, $groups) = .split(' ');
        my @groups = $groups.split(',').map: *.Int;
        $total += count-arrangements((($springs xx 5).join('?')), (@groups xx 5).flat);
    }
    say $total;
}
