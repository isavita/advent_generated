
fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let numbers: Vec<usize> = input.trim().split(' ').map(|x| x.parse().unwrap()).collect();
    
    let mut index = 0;
    let mut sum = 0;
    
    fn read_node(numbers: &Vec<usize>, index: &mut usize) -> usize {
        let num_children = numbers[*index];
        let num_metadata = numbers[*index + 1];
        *index += 2;
        
        let mut children_sum = 0;
        for _ in 0..num_children {
            children_sum += read_node(numbers, index);
        }
        
        let metadata_sum: usize = numbers[*index..*index + num_metadata].iter().sum();
        *index += num_metadata;
        
        children_sum + metadata_sum
    }
    
    sum = read_node(&numbers, &mut index);
    
    println!("{}", sum);
}
