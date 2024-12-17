
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

class Solution {

    static class Num {
        int pos;
        long val;

        Num(int pos, long val) {
            this.pos = pos;
            this.val = val;
        }
    }

    public static void main(String[] args) throws IOException {
        List<Num> nums = Files.readAllLines(Paths.get("input.txt")).stream()
                .map(Long::parseLong)
                .collect(Collectors.toList())
                .stream()
                .map(val -> new Num(0, val))
                .collect(Collectors.toList());

        List<Num> nums2 = new ArrayList<>();
        for (int i = 0; i < nums.size(); i++) {
            nums2.add(new Num(i, 811589153L * nums.get(i).val));
        }

        for (int i = 0; i < 10; i++) {
            mix(nums2);
        }
        System.out.println(coords(nums2));
    }

    static void mix(List<Num> nums) {
        int n = nums.size() - 1;
        for (int i = 0; i < nums.size(); i++) {
            int oldPos = nums.get(i).pos;
            long val = nums.get(i).val;
            int newPos = (int) (((oldPos + val) % n + n) % n);

            if (oldPos < newPos) {
                for (Num num : nums) {
                    if (num.pos > oldPos && num.pos <= newPos) {
                        num.pos--;
                    }
                }
            } else if (newPos < oldPos) {
                for (Num num : nums) {
                    if (num.pos >= newPos && num.pos < oldPos) {
                        num.pos++;
                    }
                }
            }
            nums.get(i).pos = newPos;
        }
    }

    static long coords(List<Num> nums) {
        int l = nums.size();
        int zeroPos = 0;
        for (Num num : nums) {
            if (num.val == 0) {
                zeroPos = num.pos;
                break;
            }
        }
        long sum = 0;
        for (Num num : nums) {
            if (num.pos == (zeroPos + 1000) % l || num.pos == (zeroPos + 2000) % l || num.pos == (zeroPos + 3000) % l) {
                sum += num.val;
            }
        }
        return sum;
    }
}
