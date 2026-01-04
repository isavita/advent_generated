
#!/usr/bin/perl
use strict;
use warnings;

sub trim {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    return $s;
}

sub normalize {
    my ($in) = @_;
    return { n => 0, p => [] } unless $in->{n};
    my @out;
    my ($minr, $minc) = ($in->{p}[0]{r}, $in->{p}[0]{c});
    for my $pt (@{$in->{p}}) {
        $minr = $pt->{r} if $pt->{r} < $minr;
        $minc = $pt->{c} if $pt->{c} < $minc;
    }
    for my $pt (@{$in->{p}}) {
        push @out, { r => $pt->{r} - $minr, c => $pt->{c} - $minc };
    }
    @out = sort { $a->{r} <=> $b->{r} || $a->{c} <=> $b->{c} } @out;
    return { n => scalar(@out), p => \@out };
}

sub rotateP {
    my ($a) = @_;
    my @out;
    for my $pt (@{$a->{p}}) {
        push @out, { r => $pt->{c}, c => -$pt->{r} };
    }
    return { n => scalar(@out), p => \@out };
}

sub flipP {
    my ($a) = @_;
    my @out;
    for my $pt (@{$a->{p}}) {
        push @out, { r => $pt->{r}, c => -$pt->{c} };
    }
    return { n => scalar(@out), p => \@out };
}

sub pieceEqual {
    my ($a, $b) = @_;
    return 0 unless $a->{n} == $b->{n};
    for my $i (0 .. $a->{n} - 1) {
        return 0 unless $a->{p}[$i]{r} == $b->{p}[$i]{r} && $a->{p}[$i]{c} == $b->{p}[$i]{c};
    }
    return 1;
}

sub generateVariations {
    my ($base) = @_;
    my @uniq;
    my $curr = { n => $base->{n}, p => [ map { { %$_ } } @{$base->{p}} ] };
    for my $i (0 .. 3) {
        my $n = normalize($curr);
        my $ok = 1;
        for my $u (@uniq) {
            if (pieceEqual($u, $n)) { $ok = 0; last; }
        }
        push @uniq, $n if $ok;
        my $f = flipP($curr);
        my $nf = normalize($f);
        $ok = 1;
        for my $u (@uniq) {
            if (pieceEqual($u, $nf)) { $ok = 0; last; }
        }
        push @uniq, $nf if $ok;
        my $r = rotateP($curr);
        $curr = $r;
    }
    return \@uniq;
}

sub canPlace {
    my ($rows, $cols, $grid, $p, $rr, $cc) = @_;
    for my $pt (@{$p->{p}}) {
        my ($nr, $nc) = ($rr + $pt->{r}, $cc + $pt->{c});
        return 0 if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;
        return 0 if $grid->[$nr][$nc];
    }
    return 1;
}

sub place {
    my ($cols, $grid, $p, $rr, $cc, $val) = @_;
    for my $pt (@{$p->{p}}) {
        $grid->[$rr + $pt->{r}][$cc + $pt->{c}] = $val;
    }
}

sub checkIslands {
    my ($rows, $cols, $grid, $counts, $arrSize, $slackIdx, $shapes) = @_;
    my $minReal = 1e9;
    my $hasReal = 0;
    for my $i (0 .. $arrSize - 1) {
        next if $i == $slackIdx || $counts->[$i] <= 0;
        $minReal = $shapes->[$i]{n} if $shapes->[$i]{n} < $minReal;
        $hasReal = 1;
    }
    return 1 unless $hasReal;
    my $availSlack = $counts->[$slackIdx];
    my @vis;
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            next if $grid->[$r][$c] || $vis[$r][$c];
            my @q = ([$r, $c]);
            $vis[$r][$c] = 1;
            my $size = 0;
            while (@q) {
                my ($cr, $cc) = @{shift @q};
                $size++;
                for my $d ([-1,0],[1,0],[0,-1],[0,1]) {
                    my ($nr, $nc) = ($cr + $d->[0], $cc + $d->[1]);
                    next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;
                    next if $grid->[$nr][$nc] || $vis[$nr][$nc];
                    $vis[$nr][$nc] = 1;
                    push @q, [$nr, $nc];
                }
            }
            if ($size < $minReal) {
                return 0 if $availSlack < $size;
                $availSlack -= $size;
            }
        }
    }
    return 1;
}

sub solveRec {
    my ($rows, $cols, $grid, $counts, $arrSize, $ids, $variations, $slackIdx, $shapes) = @_;
    my ($er, $ec) = (-1, -1);
    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            if (!$grid->[$r][$c]) { ($er, $ec) = ($r, $c); last; }
        }
        last if $er >= 0;
    }
    return 1 if $er < 0;
    return 0 unless checkIslands($rows, $cols, $grid, $counts, $arrSize, $slackIdx, $shapes);
    for my $ii (0 .. $#$ids) {
        my $id = $ids->[$ii];
        next unless $counts->[$id] > 0;
        $counts->[$id]--;
        for my $v (@{$variations->[$id]}) {
            next unless canPlace($rows, $cols, $grid, $v, $er, $ec);
            place($cols, $grid, $v, $er, $ec, 1);
            if (solveRec($rows, $cols, $grid, $counts, $arrSize, $ids, $variations, $slackIdx, $shapes)) {
                return 1;
            }
            place($cols, $grid, $v, $er, $ec, 0);
        }
        $counts->[$id]++;
    }
    return 0;
}

open my $fh, '<', 'input.txt' or die $!;
my @lines = map { trim($_) } <$fh>;
close $fh;

my $maxId = -1e6;
for my $l (@lines) {
    if ($l =~ /^(\d+):$/) {
        my $id = $1;
        $maxId = $id if $id > $maxId;
    }
}
$maxId = -1 if $maxId < 0;
my $arrSize = $maxId + 2;
my $slackIdx = $maxId + 1;

my @shapes;
my $parsingShapes = 1;
my $currentID = -1;
my @currentShapeLines;
my @regionLines;

for my $l (@lines) {
    next unless length $l;
    if ($l =~ /x/ && $l =~ /:/) { $parsingShapes = 0; }
    if ($parsingShapes) {
        if ($l =~ /^(\d+):$/) {
            my $id = $1;
            if ($currentID != -1 && @currentShapeLines) {
                my @pts;
                for my $r (0 .. $#currentShapeLines) {
                    my $row = $currentShapeLines[$r];
                    for my $c (0 .. length($row) - 1) {
                        push @pts, { r => $r, c => $c } if substr($row, $c, 1) eq '#';
                    }
                }
                $shapes[$currentID] = normalize({ n => scalar(@pts), p => \@pts });
            }
            @currentShapeLines = ();
            $currentID = $id;
        } else {
            push @currentShapeLines, $l;
        }
    } else {
        push @regionLines, $l;
    }
}
if ($currentID != -1 && @currentShapeLines) {
    my @pts;
    for my $r (0 .. $#currentShapeLines) {
        my $row = $currentShapeLines[$r];
        for my $c (0 .. length($row) - 1) {
            push @pts, { r => $r, c => $c } if substr($row, $c, 1) eq '#';
        }
    }
    $shapes[$currentID] = normalize({ n => scalar(@pts), p => \@pts });
}

$shapes[$_] //= { n => 0, p => [] } for 0 .. $arrSize - 1;
$shapes[$slackIdx] = { n => 1, p => [{ r => 0, c => 0 }] };

my @variations;
for my $i (0 .. $arrSize - 1) {
    if ($shapes[$i]{n}) {
        $variations[$i] = generateVariations($shapes[$i]);
    } else {
        $variations[$i] = [];
    }
}

my $solvedCount = 0;
for my $ln (@regionLines) {
    next unless $ln =~ /^(.*?)x(.*):(.*)$/;
    my ($wx, $h, $countsStr) = ($1, $2, $3);
    ($wx, $h) = (int($wx), int($h));
    my @pieceCounts = (0) x $arrSize;
    my $totalArea = 0;
    my @tok = split ' ', $countsStr;
    for my $i (0 .. $#tok) {
        my $c = int($tok[$i]);
        next unless $c > 0;
        last if $i >= $arrSize - 1;
        $pieceCounts[$i] = $c;
        $totalArea += $c * $shapes[$i]{n};
    }
    next if $totalArea > $wx * $h;
    my $slack = $wx * $h - $totalArea;
    $pieceCounts[$slackIdx] = $slack if $slack > 0;
    my @ids = grep { $pieceCounts[$_] > 0 } 0 .. $arrSize - 1;
    @ids = sort { $shapes[$b]{n} <=> $shapes[$a]{n} } @ids;
    my @grid;
    for my $r (0 .. $h - 1) {
        $grid[$r] = [ (0) x $wx ];
    }
    if (solveRec($h, $wx, \@grid, \@pieceCounts, $arrSize, \@ids, \@variations, $slackIdx, \@shapes)) {
        $solvedCount++;
    }
}

print "Number of regions that fit all presents: $solvedCount\n";
