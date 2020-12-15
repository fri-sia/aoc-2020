use std::collections::HashMap;

pub fn main() {
    let mut map: HashMap<i32, i32> = [(1, 1), (20, 2), (8,3), (12, 4), (0,5)].iter().cloned().collect();
    let mut prev_number = 14;
    let mut age = 6;
    while age != 30000000 {
        match map.get(&prev_number).cloned() {
            None => {
                map.insert(prev_number, age);
                prev_number = 0
            }
            Some(n) => {
                map.insert(prev_number, age);
                prev_number = age - n;
            }
        }
        age += 1;
    }

    println!("...nth: {}", prev_number);
}
