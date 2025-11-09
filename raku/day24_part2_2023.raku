
use v6;

class Rat { has Int $.n; has Int $.d; }

my Rat $zero = Rat.new(n => 0, d => 1);

sub gcd(Int \a, Int \b) {
    my ($a,$b)=a,b;
    while $b {
        ($a,$b)=($b,$a % $b);
    }
    $a
}

sub normalize(Rat \r) {
    my $g = gcd(r.n,r.d);
    Rat.new(n => r.n div $g, d => r.d div $g);
}

sub add(Rat \a, Rat \b) {
    normalize Rat.new(n => a.n * b.d + b.n * a.d, d => a.d * b.d);
}

sub sub(Rat \a, Rat \b) {
    normalize Rat.new(n => a.n * b.d - b.n * a.d, d => a.d * b.d);
}

sub mul(Rat \a, Rat \b) {
    normalize Rat.new(n => a.n * b.n, d => a.d * b.d);
}

sub quo(Rat \a, Rat \b) {
    normalize Rat.new(n => a.n * b.d, d => a.d * b.n);
}

sub rat-to-float(Rat \r) {
    r.n / r.d;
}

class RatVec3 { has Rat $.x; has Rat $.y; has Rat $.z; }

sub add-vec(RatVec3 \v, RatVec3 \o) {
    RatVec3.new(x => add(v.x,o.x), y => add(v.y,o.y), z => add(v.z,o.z));
}

sub sub-vec(RatVec3 \v, RatVec3 \o) {
    RatVec3.new(x => sub(v.x,o.x), y => sub(v.y,o.y), z => sub(v.z,o.z));
}

sub mul-vec(RatVec3 \v, Rat \s) {
    RatVec3.new(x => mul(v.x,s), y => mul(v.y,s), z => mul(v.z,s));
}

sub div-vec(RatVec3 \v, Rat \s) {
    RatVec3.new(x => quo(v.x,s), y => quo(v.y,s), z => quo(v.z,s));
}

sub cross-vec(RatVec3 \v, RatVec3 \o) {
    RatVec3.new(
        x => sub(mul(v.y,o.z), mul(v.z,o.y)),
        y => sub(mul(v.z,o.x), mul(v.x,o.z)),
        z => sub(mul(v.x,o.y), mul(v.y,o.x)),
    );
}

sub dot-vec(RatVec3 \v, RatVec3 \o) {
    add(mul(v.x,o.x), add(mul(v.y,o.y), mul(v.z,o.z)));
}

class HailStone { has RatVec3 $.p; has RatVec3 $.v; }

sub sub-hail(HailStone \a, HailStone \b) {
    HailStone.new(p => sub-vec(a.p,b.p), v => sub-vec(a.v,b.v));
}

sub intersection-time(HailStone \r, HailStone \s) {
    my $plane = cross-vec(r.p, add-vec(r.p,r.v));
    quo(mul(dot-vec(s.p,$plane), Rat.new(n => -1, d => 1)), dot-vec(s.v,$plane));
}

sub read-input(@lines) {
    @lines.map: -> $line {
        my @m = $line.comb(/\-?\d+/).map: *.Int;
        my $one = Rat.new(n => 1, d => 1);
        HailStone.new(
            p => RatVec3.new(
                x => Rat.new(n => @m[0], d => 1),
                y => Rat.new(n => @m[1], d => 1),
                z => Rat.new(n => @m[2], d => 1),
            ),
            v => RatVec3.new(
                x => Rat.new(n => @m[3], d => 1),
                y => Rat.new(n => @m[4], d => 1),
                z => Rat.new(n => @m[5], d => 1),
            ),
        );
    }
}

sub MAIN() {
    my @input = 'input.txt'.IO.lines;
    my @h = read-input(@input);
    my $s1 = @h[1];
    my $s2 = @h[2];
    my $ref1 = sub-hail($s1, @h[0]);
    my $ref2 = sub-hail($s2, @h[0]);

    my $t1 = intersection-time($ref2, $ref1);
    my $t2 = intersection-time($ref1, $ref2);

    my $rock1 = add-vec($s1.p, mul-vec($s1.v, $t1));
    my $rock2 = add-vec($s2.p, mul-vec($s2.v, $t2));

    my $rp = sub-vec($rock1, mul-vec(div-vec(sub-vec($rock2,$rock1), sub($t2,$t1)), $t1));
    my $sum = add($rp.x, add($rp.y, $rp.z));
    put rat-to-float($sum).Int;
}
