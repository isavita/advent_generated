
use strict;
use warnings;
use List::Util qw(sum min max);

my %KindRunes = (
    1 => '.',
    2 => 'E',
    4 => 'G',
    8 => '#',
    16 => '@',
);

my %RuneKinds = (
    '.' => 1,
    'E' => 2,
    'G' => 4,
    '#' => 8,
);

sub is_unit {
    my ($bit) = @_;
    return ($bit & (2|4)) != 0;
}

sub new_cave {
    my ($input, $elf_power) = @_;
    my $cave = {
        units => [],
        map => [],
    };
    parse_map($cave, $input, $elf_power);
    return $cave;
}


sub parse_map {
  my ($cave, $input, $elf_power) = @_;
  my $map = [];
  for my $y (0..$#{$input}) {
    my @row_chars = split //, $input->[$y];
    for my $x (0..$#row_chars) {
      my $char = $row_chars[$x];
      my $kind = $RuneKinds{$char} // 8;
      my $tile = { kind => $kind, x => $x, y => $y, unit => undef };
      if (is_unit($kind)) {
        push @{$cave->{units}}, new_unit($tile, $kind, $elf_power);
      }
      $map->[$y] ||= [];
      $map->[$y][$x] = $tile;
    }
  }
  $cave->{map} = $map;
  foreach my $row (@$map) {
      foreach my $tile (@$row) {
          $tile->{map} = $map;
      }
  }
}

sub print_map {
    my ($cave, $highlight) = @_;
    my $map = $cave->{map};
    for my $y (0..$#{$map}) {
        my @units;
        for my $x (0..$#{$map->[$y]}) {
            my $tile = $map->[$y][$x];
            if ($tile == $highlight) {
                print $KindRunes{16};
            } else {
                print $KindRunes{$tile->{kind}};
            }
            if (defined $tile->{unit}) {
                push @units, sprintf("%s(%d)", $KindRunes{$tile->{unit}->{kind}}, $tile->{unit}->{hitpoints});
            }
        }
        if (@units) {
          print "  " . join(", ", @units);
        }
        print "\n";
    }
}

sub status {
    my ($cave) = @_;
    my ($elves, $goblins) = (0,0);
    my $hp = 0;
    foreach my $unit (@{$cave->{units}}) {
        next if $unit->{hitpoints} <= 0;
        if ($unit->{kind} == 2) {
            $elves = 1;
        } else {
            $goblins = 1;
        }
        $hp += $unit->{hitpoints};
    }
    return ($hp, $elves && $goblins);
}

sub remove_the_dead {
    my ($cave) = @_;
    $cave->{units} = [grep { $_->{hitpoints} > 0 } @{$cave->{units}}];
}

sub remove_unit {
    my ($cave, $unit) = @_;
    $unit->{tile}->{kind} = 1;
    $unit->{tile}->{unit} = undef;
    $unit->{tile} = undef;
}

sub tick {
    my ($cave, $stop_on_elf_death) = @_;
    remove_the_dead($cave);
    my @units = sort {
        $a->{tile}->{y} <=> $b->{tile}->{y} || $a->{tile}->{x} <=> $b->{tile}->{x}
    } @{$cave->{units}};
    
    
    foreach my $unit (@units) {
        next if $unit->{hitpoints} <= 0;
        unless(targets($unit, $cave)){
          return (0,0);
        }
        move($unit, $cave);
        if (attack($unit, $cave) && $stop_on_elf_death) {
            return (0,1);
        }
    }
    return (1,0);
}

sub find_walkable_tiles {
    my ($tile) = @_;
    my $frontier = [$tile];
    my %distance = ($tile => 0);
    my %came_from = ($tile => undef);

    while (@$frontier) {
        my $current = shift @$frontier;
        foreach my $next (walkable_neighbors($current)) {
            unless(exists $distance{$next}){
               push @$frontier, $next;
               $distance{$next} = $distance{$current} + 1;
               $came_from{$next} = $current;
            }
        }
    }
    return (\%distance, \%came_from);
}


sub walkable_neighbors {
    my ($tile) = @_;
    my @neighbors;
    my @offsets = ([0,-1], [-1,0], [1,0], [0,1]);
    
    foreach my $offset (@offsets) {
        my ($x, $y) = ($tile->{x} + $offset->[0], $tile->{y} + $offset->[1]);
        my $neighbor = $tile->{map}->[$y] ? $tile->{map}->[$y][$x] : undef;
        if (defined $neighbor && $neighbor->{kind} == 1) {
           push @neighbors, $neighbor;
        }
    }
    return @neighbors;
}


sub new_unit {
    my ($tile, $kind, $elf_power) = @_;
    my $unit = {
        kind => $kind,
        hitpoints => 200,
        power => 3,
        tile => $tile,
    };
    $tile->{unit} = $unit;
    if ($unit->{kind} == 2) {
        $unit->{power} = $elf_power;
    }
    return $unit;
}


sub targets {
  my ($unit, $cave) = @_;
  foreach my $other_unit (@{$cave->{units}}) {
    if ($other_unit->{kind} != $unit->{kind} && $other_unit->{hitpoints} > 0) {
        return 1;
    }
  }
  return 0;
}


sub next_tile {
  my ($unit, $cave) = @_;
  my @targets;
  my $closest_target_distance = 2**31-1;
  my ($distances, $path) = find_walkable_tiles($unit->{tile});
  
    my @enemies = enemies($unit, $cave);
  
    foreach my $enemy (@enemies){
        foreach my $target (walkable_neighbors($enemy->{tile})) {
          if (exists $distances->{$target} && $distances->{$target} <= $closest_target_distance) {
              if ($distances->{$target} < $closest_target_distance){
                  $closest_target_distance = $distances->{$target};
                  @targets = ();
              }
             push @targets, $target;
          }
        }
    }
    @targets = sort {
      $a->{y} <=> $b->{y} || $a->{x} <=> $b->{x}
    } @targets;
    
  if (@targets) {
    my $target = $targets[0];
    my $current = $target;
        while (1) {
            if ($path->{$current} == $unit->{tile}) {
                return ($current, $target);
            }
            $current = $path->{$current};
        }
  }
  return (undef, undef);
}

sub enemies {
  my ($unit, $cave) = @_;
  my @enemies;
    foreach my $other_unit (@{$cave->{units}}) {
        if ($other_unit->{kind} != $unit->{kind} && $other_unit->{hitpoints} > 0) {
            push @enemies, $other_unit;
        }
    }
    @enemies = sort {
      $a->{tile}->{y} <=> $b->{tile}->{y} || $a->{tile}->{x} <=> $b->{tile}->{x}
    } @enemies;
    
  return @enemies;
}

sub enemy_neighbor {
  my ($unit, $cave) = @_;
  my $target = undef;
  my @offsets = ([0,-1], [-1,0], [1,0], [0,1]);
  foreach my $offset (@offsets) {
        my ($x, $y) = ($unit->{tile}->{x} + $offset->[0], $unit->{tile}->{y} + $offset->[1]);
      my $tile = $unit->{tile}->{map}->[$y] ? $unit->{tile}->{map}->[$y][$x] : undef;
        if (defined $tile && defined $tile->{unit} && $tile->{unit}->{kind} != $unit->{kind} && $tile->{unit}->{hitpoints} > 0) {
            if (!defined $target || $tile->{unit}->{hitpoints} < $target->{hitpoints}) {
                $target = $tile->{unit};
            }
        }
    }
    return $target;
}

sub move {
  my ($unit, $cave) = @_;
  if (defined enemy_neighbor($unit, $cave)) {
        return;
  }
  my ($next,undef) = next_tile($unit, $cave);
  if(defined $next){
    $next->{unit} = $unit;
    $next->{kind} = $unit->{kind};
    $unit->{tile}->{kind} = 1;
    $unit->{tile}->{unit} = undef;
    $unit->{tile} = $next;
  }
}

sub attack {
  my ($unit, $cave) = @_;
  my $enemy = enemy_neighbor($unit, $cave);
  if (defined $enemy) {
    my $killed = damage($enemy, $cave, $unit->{power});
    return $killed && $enemy->{kind} == 2;
  }
  return 0;
}

sub damage {
  my ($unit, $cave, $damage) = @_;
  $unit->{hitpoints} -= $damage;
  if ($unit->{hitpoints} <= 0) {
    remove_unit($cave, $unit);
    return 1;
  }
  return 0;
}

sub combat {
    my ($input) = @_;
    my $cave = new_cave($input, 3);
    for (my $i = 1; ; $i++) {
        my ($hp, $combat) = status($cave);
        if (!$combat) {
            return ($i - 1) * $hp;
        }
        my ($clean_round, undef) = tick($cave, 0);
        unless($clean_round){
           $i--;
        }
    }
}

sub cheating_elves {
    my ($input) = @_;
    my $elf_died = 1;
    for (my $power = 4; $elf_died; $power++) {
      my $cave = new_cave($input, $power);
        for (my $i = 1; ; $i++) {
            my ($hp, $combat) = status($cave);
          if (!$combat) {
             return ($i - 1) * $hp;
          }
            my ($clean_round, $elf_died) = tick($cave, 1);
            if ($elf_died) {
                last;
            }
          unless ($clean_round){
            $i--;
          }
        }
    }
}


my $file_path = "input.txt";
open my $fh, '<', $file_path or die "Could not open file '$file_path' $!";
my @lines = ();
while (my $line = <$fh>) {
    chomp $line;
    push @lines, $line;
}
close $fh;

print cheating_elves(\@lines), "\n";

