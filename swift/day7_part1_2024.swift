
   import Foundation

   func canProduce(target: Int, nums: [Int], idx: Int, current: Int) -> Bool {
       if idx == nums.count {
           return current == target
       }
       let plus = canProduce(target: target, nums: nums, idx: idx + 1, current: current + nums[idx])
       let mul = canProduce(target: target, nums: nums, idx: idx + 1, current: current * nums[idx])
       return plus || mul
   }

   do {
       let contents = try String(contentsOfFile: "input.txt")
       let lines = contents.components(separatedBy: "\n")
       var total = 0
       for line in lines {
           if line.isEmpty {
               continue
           }
           let parts = line.components(separatedBy: ":")
           guard parts.count == 2 else { continue }
           guard let target = Int(parts[0].trimmingCharacters(in: .whitespaces)) else { continue }
           let numStrs = parts[1].trimmingCharacters(in: .whitespaces).components(separatedBy: .whitespaces)
           let nums = numStrs.compactMap { Int($0) }
           if nums.count == 1 {
               if nums[0] == target {
                   total += target
               }
               continue
           }
           if canProduce(target: target, nums: nums, idx: 1, current: nums[0]) {
               total += target
           }
       }
       print(total)
   } catch {
       print("Error reading file: \(error)")
   }
   