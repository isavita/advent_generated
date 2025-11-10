
#!/usr/bin/env raku
use v6;

# ---------- constants ----------
my constant MAX   = 30;                 # size of the 4‑D grid
my constant MID   = MAX div 2;           # 15
my constant OFF   = MID - 3;             # 12  – where the input plane is placed
my constant CYCLES = 6;                  # number of simulation steps

# ---------- pre‑compute the 80 neighbour offsets ----------
my @offsets = gather {
    for -1, 0, 1 -> $dx {
        for -1, 0, 1 -> $dy {
            for -1, 0, 1 -> $dz {
                for -1, 0, 1 -> $dw {
                    next if $dx == 0 && $dy == 0 && $dz == 0 && $dw == 0;   # skip the centre cell
                    take ($dx,$dy,$dz,$dw);
                }
            }
        }
    }
}

# ---------- helpers for 4‑D <-> id conversion ----------
sub encode($x,$y,$z,$w) { ((($x * MAX) + $y) * MAX + $z) * MAX + $w }
sub decode($id) {
    my $w = $id % MAX;
    my $z = (($id div MAX) % MAX);
    my $y = (($id div (MAX*MAX)) % MAX);
    my $x =  $id div (MAX*MAX*MAX);
    return $x, $y, $z, $w;
}

# ---------- read input ----------
my @lines = "input.txt".IO.lines;                # all lines as strings
@lines = @lines.map({ .subst(/\s+/, "", :g) });   # strip any whitespace
# we only need the first 8 lines – pad the rest if the file is shorter
@lines = @lines[0..7] // @lines;
@lines = @lines.map({ $_ ~ ("." x (8 - .chars)) });   # make each line exactly 8 characters

# ---------- initial active set (hash id → True) ----------
my %active;
for 0..7 -> $i {
    for 0..7 -> $j {
        my $ch = @lines[$i].substr($j,1);
        next unless $ch eq '#';
        my $z = OFF + $i;
        my $w = OFF + $j;
        my $id = encode(MID, MID, $z, $w);
        %active{$id} = True;
    }
}

# ---------- simulation ----------
for 1..CYCLES {
    # 1️⃣ count active neighbours for every cell that could change
    my %nbr;                      # neighbour‑count hash (id → count)
    for %active.keys -> $cid {
        my ($x,$y,$z,$w) = decode($cid);
        for @offsets -> ($dx,$dy,$dz,$dw) {
            my $nx = $x + $dx;
            my $ny = $y + $dy;
            my $nz = $z + $dz;
            my $nw = $w + $dw;
            next if $nx < 0 || $nx >= MAX;
            next if $ny < 0 || $ny >= MAX;
            next if $nz < 0 || $nz >= MAX;
            next if $nw < 0 || $nw >= MAX;
            my $nid = encode($nx,$ny,$nz,$nw);
            %nbr{$nid}++;
        }
    }

    # 2️⃣ cells we have to evaluate = all cells with at least one active neighbour
    #    plus all cells that are currently active (they may have 2/3 neighbours)
    my %cand = %nbr.clone;
    for %active.keys { %cand{$_} = True; }

    # 3️⃣ apply Conway’s rules in 4‑D
    my %next;
    for %cand.keys -> $cid {
        my $is-active = %active{$cid}:exists;
        my $cnt       = %nbr{$cid} // 0;
        if $is-active {
            %next{$cid} = True if $cnt == 2 || $cnt == 3;
        } else {
            %next{$cid} = True if $cnt == 3;
        }
    }
    %active = %next;   # new generation becomes the current one
}

# ---------- answer ----------
say %active.elems;
