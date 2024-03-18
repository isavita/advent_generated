import 'dart:io';

class Num {
  int pos;
  int val;

  Num(this.pos, this.val);
}

void main() {
  List<Num> nums = [];
  List<String> lines = File('input.txt').readAsLinesSync();
  for (int i = 0; i < lines.length; i++) {
    nums.add(Num(i, int.parse(lines[i])));
  }
  
  List<Num> nums2 = List.generate(nums.length, (index) => Num(nums[index].pos, 811589153 * nums[index].val));

  for (int i = 0; i < 10; i++) {
    mix(nums2);
  }

  print(coords(nums2));
}

void mix(List<Num> nums) {
  int n = nums.length - 1;
  for (int i = 0; i < nums.length; i++) {
    int oldpos = nums[i].pos;
    int newpos = ((oldpos + nums[i].val) % n + n) % n;
    if (oldpos < newpos) {
      for (int j = 0; j < nums.length; j++) {
        if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
          nums[j].pos--;
        }
      }
    }
    if (newpos < oldpos) {
      for (int j = 0; j < nums.length; j++) {
        if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
          nums[j].pos++;
        }
      }
    }
    nums[i].pos = newpos;
  }
}

int coords(List<Num> nums) {
  int l = nums.length;
  int zeroPos = 0;
  for (int i = 0; i < nums.length; i++) {
    if (nums[i].val == 0) {
      zeroPos = nums[i].pos;
      break;
    }
  }
  int sum = 0;
  for (int i = 0; i < nums.length; i++) {
    if (nums[i].pos == (zeroPos + 1000) % l || nums[i].pos == (zeroPos + 2000) % l || nums[i].pos == (zeroPos + 3000) % l) {
      sum += nums[i].val;
    }
  }
  return sum;
}