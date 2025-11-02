
constant SIZE = 100;
constant STEPS = 100;

sub MAIN() {
    my $filename = "input.txt";
    my $fh = open $filename, :r or die "Could not open $filename: $!";
    
    # Initialize grid as flat array
    my @grid = (False) xx (SIZE * SIZE);
    my $y = 0;
    
    for $fh.lines -> $line {
        for 0 .. SIZE-1 -> $x {
            my $index = $x * SIZE + $y;
            @grid[$index] = ($line.substr($x,1) eq '#');
        }
        $y++;
    }
    $fh.close;
    
    # Initialize corners as always on
    @grid[0] = True;
    @grid[0*SIZE + SIZE-1] = True;
    @grid[(SIZE-1)*SIZE + 0] = True;
    @grid[(SIZE-1)*SIZE + SIZE-1] = True;
    
    # Simulate steps
    for 1 .. STEPS {
        my @new-grid = (False) xx (SIZE * SIZE);
        
        for 0 .. SIZE-1 -> $x {
            for 0 .. SIZE-1 -> $y {
                my $index = $x * SIZE + $y;
                my $on-neighbors = 0;
                
                # Check all 8 neighbors
                for -1 .. 1 -> $dx {
                    for -1 .. 1 -> $dy {
                        next if $dx == 0 && $dy == 0;
                        my $nx = $x + $dx;
                        my $ny = $y + $dy;
                        if 0 <= $nx < SIZE && 0 <= $ny < SIZE {
                            $on-neighbors++ if @grid[$nx * SIZE + $ny];
                        }
                    }
                }
                
                # Apply Game of Life rules
                if @grid[$index] {
                    @new-grid[$index] = ($on-neighbors == 2 || $on-neighbors == 3);
                } else {
                    @new-grid[$index] = ($on-neighbors == 3);
                }
            }
        }
        
        # Ensure corners are always on
        @new-grid[0] = True;
        @new-grid[0*SIZE + SIZE-1] = True;
        @new-grid[(SIZE-1)*SIZE + 0] = True;
        @new-grid[(SIZE-1)*SIZE + SIZE-1] = True;
        
        @grid = @new-grid;
    }
    
    # Count remaining live cells
    my $count = [+] @grid;
    say $count;
}
