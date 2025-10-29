
#!/usr/bin/env raku
use v6;

sub pattern-rows(Str $s)  { $s.split('/') }
sub rows-to-pattern(@r)  { @r.join('/') }

sub rotate(@r) {
    my $n = @r.elems;
    gather for ^$n -> $i {
        my $row = '';
        for ^$n -> $j {
            $row ~= @r[$n-1-$j].substr($i,1);
        }
        take $row;
    }
}

sub flip(@r) { @r.map( *.flip ) }

my %map;
for "input.txt".IO.lines -> $line {
    next unless $line ~~ / ' => ' /;
    my ($in,$out) = $line.split(' => ');
    my @base = pattern-rows $in;
    my @rot  = @base;
    for ^4 {
        %map{ rows-to-pattern(@rot) } = $out;
        @rot = rotate @rot;
    }
    my @fl = flip @base;
    @rot = @fl;
    for ^4 {
        %map{ rows-to-pattern(@rot) } = $out;
        @rot = rotate @rot;
    }
}

my @grid = ('.#.', '..#', '###');

for ^5 {
    my $size    = @grid.elems;
    my $sub     = $size % 2 == 0 ?? 2 !! 3;
    my $newsub  = $sub + 1;
    my $blocks  = $size div $sub;
    my @new-grid = ('') xx ($blocks * $newsub);

    for ^$blocks -> $by {
        for ^$blocks -> $bx {
            my @square;
            for ^$sub -> $dy {
                @square.push: @grid[$by*$sub+$dy].substr($bx*$sub,$sub);
            }
            my $key = rows-to-pattern @square;
            my @out = pattern-rows %map{$key};
            for ^$newsub -> $r {
                @new-grid[$by*$newsub+$r] ~= @out[$r];
            }
        }
    }
    @grid = @new-grid;
}

my $count = @grid.map({ .comb.grep('#').elems }).sum;
say $count;
