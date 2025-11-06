
use v6.d;

my constant $mod   = 1 +< 24;
my constant $steps = 2000;

sub next-secret(Int $s is copy --> Int) {
    my $x = $s * 64;
    $s +^= $x;
    $s +&= $mod - 1;
    $x = $s +> 5;                 # /32
    $s +^= $x;
    $s +&= $mod - 1;
    $x = $s * 2048;
    $s +^= $x;
    $s +&= $mod - 1;
    $s;
}

sub encode(Int \c1, Int \c2, Int \c3, Int \c4 --> Int) {
    (c1 + 9) + (c2 + 9)*19 + (c3 + 9)*19**2 + (c4 + 9)*19**3
}

sub MAIN {
    my @init = 'input.txt'.IO.lines».Int;

    my @buyers = @init.map: -> $v {
        my @p = ();
        my $s = $v;
        for ^($steps+1) -> $j {
            @p.push: $s % 10;
            $s = next-secret($s) if $j < $steps;
        }
        my @c = (1..$steps).map: { @p[$_] - @p[$_-1] };
        { :@p, :@c }
    }

    my $pat = 19**4;
    my int64 @global = 0 xx $pat;

    for @buyers -> %b {
        my int64 @local = -1 xx $pat;
        for 0..$steps-4 -> $i {
            my (\c1,\c2,\c3,\c4) = %b<c>[$i..$i+3];
            next if c1.abs > 9 || c2.abs > 9 || c3.abs > 9 || c4.abs > 9;
            my $idx = encode(c1,c2,c3,c4);
            @local[$idx] = %b<p>[$i+4] if @local[$idx] < 0;
        }
        for ^$pat -> $idx {
            @global[$idx] += @local[$idx] if @local[$idx] >= 0;
        }
    }

    say @global.max;
}
