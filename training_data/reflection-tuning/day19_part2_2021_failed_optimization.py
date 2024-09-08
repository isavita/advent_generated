from itertools import combinations, permutations
from collections import defaultdict, Counter

def parse_input(data):
    scanners = []
    for scanner in data.split('\n\n'):
        beacons = set()
        for line in scanner.split('\n')[1:]:
            beacons.add(tuple(map(int, line.split(','))))
        scanners.append(beacons)
    return scanners

def get_orientations(beacon):
    x, y, z = beacon
    return [
        ( x,  y,  z), ( x, -z,  y), ( x,  z, -y), ( x, -y, -z),
        (-x,  y, -z), (-x,  z,  y), (-x, -y,  z), (-x, -z, -y),
        ( y,  z,  x), ( y, -x,  z), ( y,  x, -z), ( y, -z, -x),
        (-y,  z, -x), (-y, -x, -z), (-y,  x,  z), (-y, -z,  x),
        ( z,  x,  y), ( z, -y,  x), ( z,  y, -x), ( z, -x, -y),
        (-z,  x, -y), (-z,  y,  x), (-z, -x,  y), (-z, -y, -x)
    ]

def find_overlap(scanner1, scanner2):
    distances1 = defaultdict(set)
    for b1, b2 in combinations(scanner1, 2):
        distance = tuple(b1[i] - b2[i] for i in range(3))
        distances1[distance].add(b1)
        distances1[distance].add(b2)
    
    for orientation in range(24):
        oriented_scanner2 = [get_orientations(b)[orientation] for b in scanner2]
        distances2 = defaultdict(set)
        for b1, b2 in combinations(oriented_scanner2, 2):
            distance = tuple(b1[i] - b2[i] for i in range(3))
            distances2[distance].add(b1)
            distances2[distance].add(b2)
        
        common_distances = set(distances1.keys()) & set(distances2.keys())
        if len(common_distances) >= 66:  # 12 choose 2
            common_beacons1 = set.union(*[distances1[d] for d in common_distances])
            common_beacons2 = set.union(*[distances2[d] for d in common_distances])
            if len(common_beacons1) >= 12 and len(common_beacons2) >= 12:
                offset = tuple(common_beacons1.pop()[i] - common_beacons2.pop()[i] for i in range(3))
                return offset, orientation
    
    return None

def align_scanners(scanners):
    aligned = {0: (set(scanners[0]), (0, 0, 0))}
    unaligned = set(range(1, len(scanners)))
    
    while unaligned:
        for i in unaligned:
            for j in aligned:
                result = find_overlap(aligned[j][0], scanners[i])
                if result:
                    offset, orientation = result
                    aligned_beacons = set(tuple(b[k] + offset[k] for k in range(3)) for b in (get_orientations(beacon)[orientation] for beacon in scanners[i]))
                    aligned[i] = (aligned_beacons, offset)
                    unaligned.remove(i)
                    break
            if i not in unaligned:
                break
    
    return aligned

def solve(data):
    scanners = parse_input(data)
    aligned = align_scanners(scanners)
    
    all_beacons = set()
    for beacons, _ in aligned.values():
        all_beacons.update(beacons)
    
    max_distance = 0
    for (_, offset1), (_, offset2) in combinations(aligned.values(), 2):
        distance = sum(abs(offset1[i] - offset2[i]) for i in range(3))
        max_distance = max(max_distance, distance)
    
    return len(all_beacons), max_distance

# Example usage:
data = """--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14"""

part1, part2 = solve(data)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
