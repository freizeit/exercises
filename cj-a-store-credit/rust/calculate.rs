pub fn find_items(i: uint, l1: &str, _l2: &str, l3: &str) -> ~str {
    let credit: uint  = from_str(l1).unwrap();
    // println!("{:?} || c1/credit = {:?}", i, credit);
    // let num_items: uint  = from_str(l2).unwrap();
    // println!("{:?} || c2/num_items = {:?}", i, num_items);
    let sitems = l3.split(' ');
    let items: ~[uint] = sitems.map(|x| from_str::<uint>(x).unwrap()).collect();
    // println!("{} || c4/items = {:?}", i, items);

    return find_solution(i, credit, items);
}

fn find_solution(i: uint, credit: uint, items: &[uint]) -> ~str {
    let mut solution = format!("Case \\#{}: no solution found", i);
    let mut x: uint = 0;
    let mut y: uint;
    while x < items.len() {
        y = x + 1;
        while y < items.len() {
            if items[x] + items[y] == credit {
                solution = format!("Case \\#{}: {} {}", i, x+1, y+1);
                break;
            }
            y += 1;
        }
        x += 1;
    }
    return solution;
}
