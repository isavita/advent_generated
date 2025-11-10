
#!/usr/bin/env raku
use v6;

# -------------------------------------------------
#  Problem: shortest number of steps needed to collect
#  all keys (`a`…`z`) and reach the exit in a grid.
#  Input is read from the file "input.txt".
# -------------------------------------------------

sub MAIN () {
    my $filename = "input.txt";
    my $fh = open $filename, :r
        or die "Cannot open $filename: $!";
    
    my @grid;                 # the maze rows (as strings)
    my Int $height = 0;       # number of rows
    my Int $width  = 0;       # number of columns (all rows have same length)
    my Int $start-x = -1;     # start column
    my Int $start-y = -1;     # start row
    my %key-index;            # map letter -> bit position (0,1,…)
    my Int $key-count = 0;    # total number of distinct keys found
    
    # ---- read the whole file -------------------------------------------------
    while defined (my $line = $fh.get) {
        $line = $line.chomp;                  # remove trailing newline
        next if $line.chars == 0;             # ignore empty lines
        
        # set the width the first time we see a non‑empty line
        $width = $line.chars if $height == 0;
        die "Inconsistent line length at row $height"
            unless $line.chars == $width;
        
        @grid[$height] = $line;               # store the row
        
        # scan the line for start position, keys, … 
        for 0 ..^ $width -> $x {
            my $c = $line.substr($x, 1);
            if $c eq '@' {
                $start-x = $x;
                $start-y = $height;
                # treat the start as an empty cell for BFS
                @grid[$height] = $line.subst('@', '.');
            }
            elsif $c ge 'a' && $c le 'z' {
                # remember the first position where this key appears
                %key-index{$c} = $key-count++ unless %key-index{$c}:exists;
            }
        }
        $height++;
    }
    $fh.close;
    
    # ---- sanity checks -------------------------------------------------------
    die "Start position '@' not found" if $start-x < 0;
    
    if $key-count == 0 {          # no keys -> we are already done
        say 0;
        return 0;
    }
    
    # ---- BFS ---------------------------------------------------------------
    my Int $target-mask = (1 +< $key-count) - 1;   # all keys collected
    
    # queue: simple array + head index (O(1) amortised pop)
    my @queue;          # each element is (x, y, mask)
    my Int $head = 0;
    my %visited;        # "x,y,mask" → Bool
    
    @queue.push( [$start-x, $start-y, 0] );
    %visited{ "$start-x,$start-y,0" } = True;
    
    my Int $steps = 0;
    my @dirs = [ [-1,0], [0,-1], [1,0], [0,1] ];   # up, left, down, right
    
    while $head < @queue.elems {
        my Int $level-size = @queue.elems - $head;
        
        for 1 .. $level-size {
            my ($x, $y, $mask) = @queue[$head++].List;
            
            # all keys collected?
            if $mask == $target-mask {
                say $steps;
                return 0;
            }
            
            for @dirs -> ($dx, $dy) {
                my Int $nx = $x + $dx;
                my Int $ny = $y + $dy;
                next unless 0 <= $nx < $width && 0 <= $ny < $height;
                
                my $cell = @grid[$ny].substr($nx, 1);
                next if $cell eq '#';            # wall
                
                my $new-mask = $mask;
                
                # door? need the corresponding key
                if $cell ge 'A' && $cell le 'Z' {
                    my $door-key = $cell.lc;
                    my $idx = %key-index{$door-key};
                    next unless $idx.defined;    # no such key in the maze
                    my $need = 1 +< $idx;
                    next unless $mask +& $need; # key not yet owned
                }
                # key? pick it up
                elsif $cell ge 'a' && $cell le 'z' {
                    my $idx = %key-index{$cell};
                    $new-mask = $mask +| (1 +< $idx) if $idx.defined;
                }
                
                my $state-key = "$nx,$ny,$new-mask";
                next if %visited{$state-key}:exists;
                %visited{$state-key} = True;
                @queue.push( [$nx, $ny, $new-mask] );
            }
        }
        $steps++;
    }
    
    # no path found (should not happen for well‑formed inputs)
    say -1;
    return 0;
}
