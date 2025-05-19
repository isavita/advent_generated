
use v6;
use Algorithm::Heap;

sub MAIN {
    my %grid;
    my ($start, $end);

    my $y = 0;
    for 'input.txt'.IO.lines -> $line {
        for $line.chomp.comb.indexed -> $x, $char {
            my $p = $x => $y;
            %grid{$p} = $char;
            if $char eq 'S' {
                $start = $p;
            } elsif $char eq 'E' {
                $end = $p;
            }
        }
        $y++;
    }

    %grid{$start} = 'a';
    %grid{$end} = 'z';

    my %dist;
    %dist{$end} = 0;

    my $heap = Algorithm::Heap.new( :&cmp{ $^a.key <=> $^b.key } );
    $heap.push(0 => $end);

    my @neighbors = (-1, 0), (1, 0), (0, -1), (0, 1);

    while $heap.elems > 0 {
        my $curr-dist => $curr = $heap.pop;

        if %dist{$curr}.defined && $curr-dist > %dist{$curr} {
            next;
        }

        for @neighbors -> $dx, $dy {
            my $neighbor = ($curr.key + $dx) => ($curr.value + $dy);

            if %grid{$neighbor}.defined {
                if %grid{$curr}.ord - %grid{$neighbor}.ord <= 1 {
                    my $new-dist = %dist{$curr} + 1;

                    if !%dist{$neighbor}.defined || $new-dist < %dist{$neighbor} {
                        %dist{$neighbor} = $new-dist;
                        $heap.push($new-dist => $neighbor);
                    }
                }
            }
        }
    }

    print %dist{$start};
}

