
my $rock-str = '####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##';

my @jet = 'input.txt'.IO.slurp.trim.comb;
my @rocks = get-rocks();
my %grid;
for ^7 { %grid{"$_,0"} = True }

my $floor = 0;
my $j = 0;
my %repeat;

for ^2022 -> $i {
    my $curr = $i % +@rocks;
    my $key = "$curr,$j";
    %repeat{$key} = $i, $floor;
    my %curr-rock = @rocks[$curr];
    my %pos = x => 2, y => $floor + 4;
    loop {
        my $jet = @jet[$j];
        $j = ($j + 1) % +@jet;
        %pos = add-pos(%pos, dir-from-byte($jet));
        %pos = add-pos(%pos, reverse-dir(dir-from-byte($jet))) if collision(%grid, %curr-rock, %pos);
        %pos = add-pos(%pos, { x => 0, y => -1 });
        if collision(%grid, %curr-rock, %pos) {
            %pos = add-pos(%pos, { x => 0, y => 1 });
            for %curr-rock.keys -> $p {
                my ($dx,$dy) = $p.split(',');
                my $key = "{ $dx + %pos<x> }," ~ "{ $dy + %pos<y> }";
                %grid{$key} = True;
                $floor = max($floor, $dy + %pos<y>);
            }
            last;
        }
    }
}
put $floor;

sub get-rocks {
    my @rocks;
    my @rock-lines = $rock-str.split("\n\n");
    for @rock-lines -> $rock {
        my %rock;
        my @lines = $rock.lines;
        for @lines.kv -> $y,$line {
            for $line.comb.kv -> $x,$ch {
                %rock{"$x,{@lines - 1 - $y}"} = True if $ch eq '#';
            }
        }
        @rocks.push(%rock);
    }
    @rocks
}

sub dir-from-byte(Str $b) { $b eq '>' ?? { x => 1, y => 0 } !! { x => -1, y => 0 } }

sub reverse-dir(%d) { { x => -%d<x>, y => -%d<y> } }

sub add-pos(%a, %b) { { x => %a<x> + %b<x>, y => %a<y> + %b<y> } }

sub collision(%grid, %rock, %pos) {
    for %rock.keys -> $p {
        my ($dx,$dy) = $p.split(',');
        my $x = $dx + %pos<x>;
        my $y = $dy + %pos<y>;
        return True if %grid{"$x,$y"} or $x < 0 or $x > 6;
    }
    False
}
