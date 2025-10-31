
use v6;

sub MAIN () {
    my %dir = '^' => (0, -1), 'v' => (0, 1), '<' => (-1, 0), '>' => (1, 0);
    my %left = '^' => '<', '<' => 'v', 'v' => '>', '>' => '^';
    my %right = '^' => '>', '<' => '^', 'v' => '<', '>' => 'v';
    my @track = slurp('input.txt').lines;
    my $max-x = @track>>.chars.max;
    my $max-y = @track.elems;
    my @grid = (^$max-y).map: { [0 xx $max-x] };
    my %carts;
    my $id = 1;
    for ^$max-y -> $y {
        for ^@track[$y].chars -> $x {
            my $ch = @track[$y].substr($x,1);
            if %dir{$ch}:exists {
                %carts{$id} = { id => $id, x => $x, y => $y,
                               dir => $ch, turn => 0 };
                @grid[$y][$x] = $id;
                @track[$y] = @track[$y].substr(0,$x) ~
                            ( $ch eq '^' || $ch eq 'v' ?? '|' !! '-' ) ~
                            @track[$y].substr($x+1);
                $id++;
            }
        }
    }
    loop {
        my @order = %carts.keys.sort: {
            %carts{$^a}<y> <=> %carts{$^b}<y> ||
            %carts{$^a}<x> <=> %carts{$^b}<x>
        };
        for @order -> $cid {
            next unless %carts{$cid}:exists;
            my $c = %carts{$cid};
            @grid[$c<y>][$c<x>] = 0;
            my ($dx,$dy) = %dir{$c<dir>};
            $c<x> += $dx;
            $c<y> += $dy;
            if @grid[$c<y>][$c<x>] != 0 {
                say "$c<x>,$c<y>";
                exit;
            }
            given @track[$c<y>].substr($c<x>,1) {
                when '+' {
                    if $c<turn> == 0 {
                        $c<dir> = %left{$c<dir>};
                        $c<turn> = 1;
                    } elsif $c<turn> == 1 {
                        $c<turn> = 2;
                    } else {
                        $c<dir> = %right{$c<dir>};
                        $c<turn> = 0;
                    }
                }
                when '/' {
                    $c<dir> = $c<dir> eq '>'  ?? '^' !!
                               $c<dir> eq 'v'  ?? '<' !!
                               $c<dir> eq '<'  ?? 'v' !!
                               '>';
                }
                when '\\' {
                    $c<dir> = $c<dir> eq '>'  ?? 'v' !!
                               $c<dir> eq 'v'  ?? '>' !!
                               $c<dir> eq '<'  ?? '^' !!
                               '<';
                }
            }
            %carts{$cid} = $c;
            @grid[$c<y>][$c<x>] = $cid;
        }
    }
}
