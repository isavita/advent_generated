
use v6;

class Blueprint {
    has Int $.id;
    has Int $.ore-for-ore-robot;
    has Int $.ore-for-clay-robot;
    has Int $.ore-for-obsidian-robot;
    has Int $.clay-for-obsidian-robot;
    has Int $.ore-for-geode-robot;
    has Int $.obsidian-for-geode-robot;
}

class State {
    has Blueprint $.blueprint;
    has Int $.ore is rw = 0;
    has Int $.clay is rw = 0;
    has Int $.obsidian is rw = 0;
    has Int $.geode is rw = 0;
    has Int $.ore-robots is rw = 1;
    has Int $.clay-robots is rw = 0;
    has Int $.obsidian-robots is rw = 0;
    has Int $.geode-robots is rw = 0;

    method farm() {
        $!ore += $!ore-robots;
        $!clay += $!clay-robots;
        $!obsidian += $!obsidian-robots;
        $!geode += $!geode-robots;
    }

    method hash(Int $time --> Str) {
        "$time,$!ore,$!clay,$!obsidian,$!geode,$!ore-robots,$!clay-robots,$!obsidian-robots,$!geode-robots"
    }

    method calc-most-geodes(Int $time, %memo, Int $total-time, Int $earliest-geode is rw --> Int) {
        return $!geode if $time == $total-time;

        my $h = self.hash($time);
        return %memo{$h} if %memo{$h}:exists;

        return 0 if $!geode == 0 && $time > $earliest-geode;

        my Int $most-geodes = $!geode;

        if $!ore >= $!blueprint.ore-for-geode-robot && $!obsidian >= $!blueprint.obsidian-for-geode-robot {
            my $cp = self.clone;
            $cp.farm;
            $cp.ore -= $!blueprint.ore-for-geode-robot;
            $cp.obsidian -= $!blueprint.obsidian-for-geode-robot;
            $cp.geode-robots += 1;
            $earliest-geode min= $time + 1 if $cp.geode-robots == 1;
            $most-geodes max= $cp.calc-most-geodes($time + 1, %memo, $total-time, $earliest-geode);
        }

        if $time <= $total-time - 16 && $!ore-robots < $!blueprint.ore-for-obsidian-robot * 2 && $!ore >= $!blueprint.ore-for-ore-robot {
            my $cp = self.clone;
            $cp.ore -= $!blueprint.ore-for-ore-robot;
            $cp.farm;
            $cp.ore-robots += 1;
            $most-geodes max= $cp.calc-most-geodes($time + 1, %memo, $total-time, $earliest-geode);
        }

        if $time <= $total-time - 8 && $!clay-robots < $!blueprint.clay-for-obsidian-robot && $!ore >= $!blueprint.ore-for-clay-robot {
            my $cp = self.clone;
            $cp.ore -= $!blueprint.ore-for-clay-robot;
            $cp.farm;
            $cp.clay-robots += 1;
            $most-geodes max= $cp.calc-most-geodes($time + 1, %memo, $total-time, $earliest-geode);
        }

        if $time <= $total-time - 4 && $!obsidian-robots < $!blueprint.obsidian-for-geode-robot && $!ore >= $!blueprint.ore-for-obsidian-robot && $!clay >= $!blueprint.clay-for-obsidian-robot {
            my $cp = self.clone;
            $cp.ore -= $!blueprint.ore-for-obsidian-robot;
            $cp.clay -= $!blueprint.clay-for-obsidian-robot;
            $cp.farm;
            $cp.obsidian-robots += 1;
            $most-geodes max= $cp.calc-most-geodes($time + 1, %memo, $total-time, $earliest-geode);
        }

        my $cp = self.clone;
        $cp.farm;
        $most-geodes max= $cp.calc-most-geodes($time + 1, %memo, $total-time, $earliest-geode);

        %memo{$h} = $most-geodes;
        return $most-geodes;
    }
}

sub parse-input(Str $input --> Seq) {
    $input.lines.map: -> $line {
        my ($id, $oo, $oc, $obo, $obc, $ogo, $obg) = $line.match(/ \d+ /, :g).map: *.Int;
        Blueprint.new(:$id, :ore-for-ore-robot($oo), :ore-for-clay-robot($oc),
                       :ore-for-obsidian-robot($obo), :clay-for-obsidian-robot($obc),
                       :ore-for-geode-robot($ogo), :obsidian-for-geode-robot($obg));
    }
}

sub MAIN() {
    my $input = slurp 'input.txt';
    my @blueprints = parse-input($input);

    my Int $sum = 0;
    for @blueprints -> $bp {
        my State $st = State.new(:blueprint($bp));
        my %memo;
        my Int $earliest = 24;
        my Int $geodes = $st.calc-most-geodes(0, %memo, 24, $earliest);
        $sum += $bp.id * $geodes;
    }

    say $sum;
}
