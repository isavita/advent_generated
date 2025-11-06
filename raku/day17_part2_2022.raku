
my $rockstr = '####

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

sub get-rocks {
    my @rocks;
    for $rockstr.split("\n\n") -> $block {
        my %rock;
        my @lines = $block.lines;
        for @lines.kv -> $y,$line {
            for $line.comb.kv -> $x,$c {
                %rock{"$x,{@lines.elems - 1 - $y}"} = True if $c eq '#';
            }
        }
        @rocks.push: %rock;
    }
    @rocks
}

sub collision(%grid, %rock, %pos) {
    for %rock.keys -> $key {
        my ($x,$y) = $key.split(',');
        my $nx = $x + %pos<x>;
        my $ny = $y + %pos<y>;
        return True if $nx < 0 || $nx > 6 || %grid{"$nx,$ny"};
    }
    False
}

sub MAIN {
    my @jet = 'input.txt'.IO.slurp.trim.comb;
    my @rocks = get-rocks();
    my %grid;
    %grid{"$_,0"} = True for ^7;
    my $floor = 0;
    my $j = 0;
    my %repeat;

    loop (my $i = 0, my $curr = 0; ; $i++, $curr = ($curr + 1) % @rocks) {
        my $key = "$curr,$j";
        if %repeat{$key}:exists {
            my ($previ,$prev-floor) = %repeat{$key};
            if (1_000_000_000_000 - $i) % ($i - $previ) == 0 {
                say $floor + (1_000_000_000_000 - $i) div ($i - $previ) * ($floor - $prev-floor);
                exit;
            }
        }
        %repeat{$key} = [$i, $floor];
        my %rock := @rocks[$curr];
        my %pos = x => 2, y => $floor + 4;
        loop {
            my $dir = @jet[$j] eq '<' ?? -1 !! 1;
            $j = ($j + 1) % @jet;
            %pos<x> += $dir;
            %pos<x> -= $dir if collision(%grid, %rock, %pos);
            %pos<y>--;
            if collision(%grid, %rock, %pos) {
                %pos<y>++;
                for %rock.keys -> $key {
                    my ($x,$y) = $key.split(',');
                    my $gx = $x + %pos<x>;
                    my $gy = $y + %pos<y>;
                    %grid{"$gx,$gy"} = True;
                    $floor = $gy if $gy > $floor;
                }
                last;
            }
        }
    }
}
