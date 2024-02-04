
import re

def jumbled_seven_segment(input_data):
    parsed_input = []
    for line in input_data.split("\n"):
        parts = re.findall(r'[a-g]+', line)
        if len(parts) != 14:
            raise ValueError("should be 14 parts in each input line")
        
        fourteen = ["".join(sorted(v)) for v in parts]
        parsed_input.append(fourteen)
    
    ans = 0
    index_to_characters = [""] * 10
    for set in parsed_input:
        working_set = set[:10]
        kill_indices = []
        
        for i, mapping in enumerate(working_set):
            if len(mapping) == 2:
                index_to_characters[1] = mapping
                kill_indices.append(i)
            elif len(mapping) == 4:
                index_to_characters[4] = mapping
                kill_indices.append(i)
            elif len(mapping) == 3:
                index_to_characters[7] = mapping
                kill_indices.append(i)
            elif len(mapping) == 7:
                index_to_characters[8] = mapping
                kill_indices.append(i)
        
        working_set = [v for i, v in enumerate(working_set) if i not in kill_indices]
        
        zero_three_or_nine = []
        kill_indices = []
        for i, mapping in enumerate(working_set):
            if check_string_overlap(mapping, index_to_characters[1]):
                zero_three_or_nine.append(mapping)
                kill_indices.append(i)
        if len(zero_three_or_nine) != 3:
            raise ValueError("one three or nine does not have three matches")
        
        for i, maybe039 in enumerate(zero_three_or_nine):
            if len(maybe039) == 5:
                index_to_characters[3] = maybe039
                zero_three_or_nine.pop(i)
                break
        
        for i, maybe09 in enumerate(zero_three_or_nine):
            if check_string_overlap(maybe09, index_to_characters[4]):
                index_to_characters[9] = maybe09
                zero_three_or_nine.pop(i)
        
        index_to_characters[0] = zero_three_or_nine[0]
        
        working_set = [v for i, v in enumerate(working_set) if i not in kill_indices]
        if len(working_set) != 3:
            raise ValueError("expected length of 3 at this stage")
        
        for i, mapping in enumerate(working_set):
            if len(mapping) == 6:
                index_to_characters[6] = mapping
                working_set.pop(i)
        
        for i, mapping in enumerate(working_set):
            if check_string_overlap(index_to_characters[9], mapping):
                index_to_characters[5] = mapping
                working_set.pop(i)
        
        if len(working_set) != 1:
            raise ValueError("expected length of 1 at this stage")
        
        index_to_characters[2] = working_set[0]
        
        num = 0
        for out in set[10:]:
            for i, mapping in enumerate(index_to_characters):
                if out == mapping:
                    num *= 10
                    num += i
        ans += num
    
    return ans

def remove_slice_indices(sli, *indices):
    ans = [v for i, v in enumerate(sli) if i not in indices]
    return ans

def check_string_overlap(larger, smaller):
    if len(larger) < len(smaller):
        larger, smaller = smaller, larger
    
    large_map = {r: True for r in larger}
    for r in smaller:
        if r not in large_map:
            return False
    return True

def alphabetize_string(input_str):
    return "".join(sorted(input_str))

if __name__ == "__main__":
    with open("input.txt", "r") as file:
        input_text = file.read().strip()
    
    answer = jumbled_seven_segment(input_text)
    print(answer)
