#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(any);

sub solve {
    my $input = shift;
    my @tiles = parse_tiles($input);
    my $edge = sqrt(scalar(@tiles));
    my @assembled = map { [ (undef) x $edge ] } (1..$edge);
    my %used;
    my $res = backtrack_assemble(\@tiles, \@assembled, \%used, $edge, 0);
    die "No solution" unless defined $res;
    for my $r (0..$edge-1) {
        for my $c (0..$edge-1) {
            $res->[$r][$c]{contents} = remove_borders($res->[$r][$c]{contents});
        }
    }
    my @image;
    my $tile_h = scalar(@{$res->[0][0]{contents}});
    for my $big_r (0..$edge-1) {
        for my $sub_r (0..$tile_h-1) {
            my @row;
            for my $big_c (0..$edge-1) {
                push @row, @{$res->[$big_r][$big_c]{contents}[$sub_r]};
            }
            push @image, \@row;
        }
    }
    my $final;
    my @orients = all_orientations(\@image);
    my @mons;
    for my $g (@orients) {
        @mons = find_monster_coords($g);
        if (@mons) { $final = $g; last; }
    }
    die "No monster" unless defined $final;
    $_->[0] = $_->[0] foreach @mons;  # dummy, to satisfy strict; not used
    for my $coord (@mons) {
        my ($r, $c) = @$coord;
        $final->[$r][$c] = 'O';
    }
    my $count = 0;
    for my $r (0..$#$final) {
        for my $c (0..$#{$final->[$r]}) {
            $count++ if $final->[$r][$c] eq '#';
        }
    }
    return $count;
}

sub parse_tiles {
    my $input = shift;
    my @tiles;
    for my $block (split /\n\s*\n/, $input) {
        my @lines = split /\n/, $block;
        $lines[0] =~ /Tile (\d+):/ or die;
        my $id = $1;
        my @grid;
        for my $line (@lines[1..$#lines]) {
            push @grid, [ split //, $line ];
        }
        push @tiles, { id => $id, contents => \@grid };
    }
    return @tiles;
}

sub backtrack_assemble {
    my ($tiles, $assembled, $used, $edge, $pos) = @_;
    return $assembled if $pos == $edge * $edge;
    my $row = int($pos / $edge);
    my $col = $pos % $edge;
    for my $i (0..$#$tiles) {
        next if $used->{$i};
        my $tile = $tiles->[$i];
        for my $grid ( all_orientations($tile->{contents}) ) {
            if ($row > 0) {
                my $above = $assembled->[$row-1][$col]{contents};
                my $above_bottom = join("", @{$above->[-1]});
                my $cur_top = join("", @{$grid->[0]});
                next if $above_bottom ne $cur_top;
            }
            if ($col > 0) {
                my $left = $assembled->[$row][$col-1]{contents};
                my $left_right = join("", map { $_->[-1] } @$left);
                my $cur_left = join("", map { $_->[0] } @$grid);
                next if $left_right ne $cur_left;
            }
            $assembled->[$row][$col] = { id => $tile->{id}, contents => $grid };
            $used->{$i} = 1;
            my $res = backtrack_assemble($tiles, $assembled, $used, $edge, $pos+1);
            return $res if defined $res;
            $assembled->[$row][$col] = undef;
            delete $used->{$i};
        }
    }
    return undef;
}

sub remove_borders {
    my $grid = shift;
    my @new;
    for my $i (1 .. $#$grid - 1) {
        my @row = @{$grid->[$i]};
        push @new, [ @row[1 .. $#row - 1] ];
    }
    return \@new;
}

sub find_monster_coords {
    my $image = shift;
    my @monster = split /\n/, <<'EOF';
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
EOF
    chomp @monster;
    my @offsets;
    my $m_h = scalar(@monster);
    my $m_w = length($monster[0]);
    for my $r (0..$#monster) {
        my @chars = split //, $monster[$r];
        for my $c (0..$#chars) {
            push @offsets, [$r, $c] if $chars[$c] eq '#';
        }
    }
    my @found;
    for my $r (0 .. scalar(@$image) - $m_h) {
        for my $c (0 .. scalar(@{$image->[0]}) - $m_w) {
            my $ok = 1;
            for my $off (@offsets) {
                my ($dr, $dc) = @$off;
                if ($image->[$r+$dr][$c+$dc] ne '#') { $ok = 0; last; }
            }
            push @found, map { [$r+$_->[0], $c+$_->[1]] } @offsets if $ok;
        }
    }
    return @found;
}

sub all_orientations {
    my $grid = shift;
    my @ors;
    my $g = $grid;
    for (1..4) {
        push @ors, $g;
        $g = rotate_grid($g);
    }
    for my $i (0..3) {
        push @ors, mirror_grid($ors[$i]);
    }
    return @ors;
}

sub rotate_grid {
    my $grid = shift;
    my $rows = scalar(@$grid);
    my $cols = scalar(@{$grid->[0]});
    my @rot;
    for my $j (0..$cols-1) {
        my @new;
        for my $i (reverse 0..$rows-1) {
            push @new, $grid->[$i][$j];
        }
        push @rot, \@new;
    }
    return \@rot;
}

sub mirror_grid {
    my $grid = shift;
    my @new = map { [ reverse @$_ ] } @$grid;
    return \@new;
}

my $input = do { local $/; open(my $fh, '<', 'input.txt') or die $!; <$fh> };
$input =~ s/^\s+|\s+$//g;
print solve($input), "\n";