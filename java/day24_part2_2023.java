
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day24 {

    static class RatVec3 {
        BigDecimal X;
        BigDecimal Y;
        BigDecimal Z;

        public RatVec3(BigDecimal x, BigDecimal y, BigDecimal z) {
            this.X = x;
            this.Y = y;
            this.Z = z;
        }

        public RatVec3 add(RatVec3 other) {
            return new RatVec3(this.X.add(other.X), this.Y.add(other.Y), this.Z.add(other.Z));
        }

        public RatVec3 subtract(RatVec3 other) {
            return new RatVec3(this.X.subtract(other.X), this.Y.subtract(other.Y), this.Z.subtract(other.Z));
        }

        public RatVec3 multiply(BigDecimal s) {
            return new RatVec3(this.X.multiply(s), this.Y.multiply(s), this.Z.multiply(s));
        }

        public RatVec3 divide(BigDecimal s) {
            return new RatVec3(this.X.divide(s, 20, RoundingMode.HALF_UP), this.Y.divide(s, 20, RoundingMode.HALF_UP), this.Z.divide(s, 20, RoundingMode.HALF_UP));
        }

        public RatVec3 cross(RatVec3 other) {
            return new RatVec3(this.Y.multiply(other.Z).subtract(this.Z.multiply(other.Y)),
                    this.Z.multiply(other.X).subtract(this.X.multiply(other.Z)),
                    this.X.multiply(other.Y).subtract(this.Y.multiply(other.X)));
        }

        public BigDecimal dot(RatVec3 other) {
            return this.X.multiply(other.X).add(this.Y.multiply(other.Y)).add(this.Z.multiply(other.Z));
        }
    }

    static class HailStone {
        RatVec3 p;
        RatVec3 v;

        public HailStone(RatVec3 p, RatVec3 v) {
            this.p = p;
            this.v = v;
        }

        public HailStone subtract(HailStone other) {
            return new HailStone(this.p.subtract(other.p), this.v.subtract(other.v));
        }
    }

    public static void main(String[] args) {
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            List<String> input = new ArrayList<>();
            String line;
            while ((line = br.readLine()) != null) {
                input.add(line);
            }
            System.out.println(solve(input));

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String solve(List<String> input) {
        List<HailStone> hailStones = readInput(input.subList(0, 3));
        HailStone s1 = hailStones.get(1);
        HailStone s2 = hailStones.get(2);
        HailStone ref1 = s1.subtract(hailStones.get(0));
        HailStone ref2 = s2.subtract(hailStones.get(0));

        BigDecimal t1 = intersectionTime(ref2, ref1);
        BigDecimal t2 = intersectionTime(ref1, ref2);

        RatVec3 rock1 = s1.p.add(s1.v.multiply(t1));
        RatVec3 rock2 = s2.p.add(s2.v.multiply(t2));

        RatVec3 rp = rock1.subtract(rock2.subtract(rock1).divide(t2.subtract(t1)).multiply(t1));
        return String.valueOf(rp.X.add(rp.Y).add(rp.Z).longValue());
    }

    public static List<HailStone> readInput(List<String> input) {
        List<HailStone> res = new ArrayList<>();
        for (String line : input) {
            res.add(readLine(line));
        }
        return res;
    }

    public static HailStone readLine(String line) {
        Pattern pattern = Pattern.compile("-?\\d+");
        Matcher matcher = pattern.matcher(line);
        List<BigDecimal> numbers = new ArrayList<>();
        while (matcher.find()) {
            numbers.add(new BigDecimal(matcher.group()));
        }
        return new HailStone(new RatVec3(numbers.get(0), numbers.get(1), numbers.get(2)),
                new RatVec3(numbers.get(3), numbers.get(4), numbers.get(5)));
    }

    public static BigDecimal intersectionTime(HailStone r, HailStone s) {
        RatVec3 plane = r.p.cross(r.p.add(r.v));
        return s.p.dot(plane).negate().divide(s.v.dot(plane));
    }
}
