
class Cube {
    has Bool $.is-on;
    has Int $.x1; has Int $.x2;
    has Int $.y1; has Int $.y2;
    has Int $.z1; has Int $.z2;

    method new(Bool $on, Int $a, Int $b, Int $c, Int $d, Int $e, Int $f) {
        self.bless(:is-on($on), :x1($a), :x2($b), :y1($c), :y2($d), :z1($e), :z2($f))
    }

    method intersect(Cube $c2) {
        my Int $nx1 = self.x1 max $c2.x1;
        my Int $nx2 = self.x2 min $c2.x2;
        my Int $ny1 = self.y1 max $c2.y1;
        my Int $ny2 = self.y2 min $c2.y2;
        my Int $nz1 = self.z1 max $c2.z1;
        my Int $nz2 = self.z2 min $c2.z2;
        return (Cube.new(False, 0,0,0,0,0,0), False) if $nx1 > $nx2 || $ny1 > $ny2 || $nz1 > $nz2;
        my Bool $st = (self.is-on && $c2.is-on) ?? False !! (!self.is-on && !$c2.is-on) ?? True !! $c2.is-on;
        (Cube.new($st, $nx1, $nx2, $ny1, $ny2, $nz1, $nz2), True)
    }

    method volume() {
        my Int $v = (self.x2 - self.x1 + 1) * (self.y2 - self.y1 + 1) * (self.z2 - self.z1 + 1);
        self.is-on ?? $v !! -$v
    }
}

sub parse(Str $in) {
    $in.lines.map: -> $l {
        my ($cmd, $rest) = $l.split(' ');
        my ($x1,$x2,$y1,$y2,$z1,$z2) = $rest.comb(/ '-'? \d+ /).map: *.Int;
        Cube.new($cmd eq 'on', $x1,$x2,$y1,$y2,$z1,$z2)
    }
}

sub MAIN {
    my @cubes = parse('input.txt'.IO.slurp);
    my @final;
    for @cubes -> $c {
        my @add;
        for @final -> $f {
            my ($i, $hit) = $f.intersect($c);
            @add.push($i) if $hit;
        }
        @add.push($c) if $c.is-on;
        @final.append: @add;
    }
    say [+] @final.map: *.volume
}
