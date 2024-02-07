
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Solution {
    static class Num {
        int pos;
        int val;

        Num(int pos, int val) {
            this.pos = pos;
            this.val = val;
        }
    }

    public static void main(String[] args) {
        List<Num> nums = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("input.txt"))) {
            String line;
            int i = 0;
            while ((line = br.readLine()) != null) {
                nums.add(new Num(i, Integer.parseInt(line)));
                i++;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        List<Num> nums2 = new ArrayList<>(nums.size());
        for (Num num : nums) {
            nums2.add(new Num(num.pos, 811589153 * num.val));
        }

        mix(nums);

        System.out.println(coords(nums));
    }

    static void mix(List<Num> nums) {
        int n = nums.size() - 1;
        for (Num num : nums) {
            int oldPos = num.pos;
            int newPos = ((oldPos + num.val) % n + n) % n;
            if (oldPos < newPos) {
                for (Num num2 : nums) {
                    if (num2.pos > oldPos && num2.pos <= newPos) {
                        num2.pos--;
                    }
                }
            }
            if (newPos < oldPos) {
                for (Num num2 : nums) {
                    if (num2.pos >= newPos && num2.pos < oldPos) {
                        num2.pos++;
                    }
                }
            }
            num.pos = newPos;
        }
    }

    static int coords(List<Num> nums) {
        int l = nums.size();
        int zeroPos = 0;
        for (Num num : nums) {
            if (num.val == 0) {
                zeroPos = num.pos;
                break;
            }
        }
        int sum = 0;
        for (Num num : nums) {
            if (num.pos == (zeroPos + 1000) % l || num.pos == (zeroPos + 2000) % l || num.pos == (zeroPos + 3000) % l) {
                sum += num.val;
            }
        }
        return sum;
    }
}
