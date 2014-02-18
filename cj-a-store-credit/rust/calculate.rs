pub fn find_items(i: uint, l1: &str, l2: &str, l3: &str) -> ~str {
    let credit: uint  = from_str(l1).unwrap();
    println!("{:?} || c1/credit = {:?}", i, credit);
    let num_items: uint  = from_str(l2).unwrap();
    println!("{:?} || c2/num_items = {:?}", i, num_items);
    let sitems = l3.split(' ');
    let items: ~[uint] = sitems.map(|x| from_str::<uint>(x).unwrap()).collect();
    println!("{} || c4/items = {:?}", i, items);

    return format!("{}", i) + " -> " + l1 + " || " + l2;
}
