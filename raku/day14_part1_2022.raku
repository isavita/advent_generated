
my constant MAX-GRID = 1000;
my @grid;
my int $floor = 0;

sub populate(int \x1,int \y1,int \x2,int \y2) {
    if x1 == x2 {
        for min(y1,y2)..max(y1,y2) -> \y {
            @grid[x1][y] = True;
            $floor = max($floor, y);
        }
    } else {
        for min(x1,x2)..max(x1,x2) -> \x {
            @grid[x][y1] = True;
            $floor = max($floor, y1);
        }
    }
}

sub fill() {
    $floor += 1;
    my int $sands = 0;
    my int $first = 0;
    while !@grid[500][0] {
        my int ($x,$y) = 500,0;
        my Bool $settled = False;
        while !$settled {
            my @next = ($x,$y+1), ($x-1,$y+1), ($x+1,$y+1);
            my Bool $moved = False;
            for @next -> (\nx,\ny) {
                if !@grid[nx][ny] {
                    ($x,$y) = nx,ny;
                    $moved = True;
                    last;
                }
            }
            if !$moved {
                @grid[$x][$y] = True;
                $settled = True;
            }
            if $y == $floor {
                $first = $first || $sands;
                @grid[$x][$y] = True;
                $settled = True;
            }
        }
        $sands += 1;
    }
    $first
}

@grid = [[False xx MAX-GRID] xx MAX-GRID];
for 'input.txt'.IO.lines {
    my @pts = .split(' -> ');
    my ($x,$y) = @pts.shift.split(',').map: *.Int;
    for @pts {
        my ($nx,$ny) = .split(',').map: *.Int;
        populate($x,$y,$nx,$ny);
        ($x,$y) = $nx,$ny;
    }
}
say fill();
