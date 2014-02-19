//!    This module provides logic for solving the credit store problem.
//!    Please see the link below for details.
//!
//!        http://code.google.com/codejam/contest/dashboard?c=351101#s=p0


pub fn find_items(i: uint, l1: &str, _l2: &str, l3: &str) -> ~str {
    let credit: uint  = from_str(l1).unwrap();
    let sitems = l3.split(' ');
    let items: ~[uint] = sitems.map(|x| from_str::<uint>(x).unwrap()).collect();

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


#[test]
fn no_solution_found() {
    let res = find_items(11, "22", "3", "1 2 3");
    assert!("Case #11: no solution found" == res);
}


#[test]
fn solution_found_at_start_and_end() {
    let res = find_items(12, "25", "4", "11 2 3 14");
    assert!("Case #12: 1 4" == res);
}


#[test]
fn solution_found_at_left_boundary() {
    let res = find_items(13, "26", "5", "11 15 2 3 14");
    assert!("Case #13: 1 2" == res);
}


#[test]
fn solution_found_at_right_boundary() {
    let res = find_items(14, "2006", "6", "11 15 2 3 14 1992");
    assert!("Case #14: 5 6" == res);
}


#[test]
fn solution_found_adjacent_in_the_middle() {
    let res = find_items(15, "99999", "7", "11 15 99990 9 1 14 1992");
    assert!("Case #15: 3 4" == res);
}


#[test]
fn solution_found_nonadjacent_in_the_middle() {
    let res = find_items(16, "1234567", "11", "11111111 1 15 99990 9 888888888888888 1 1134577 1992 10 11");
    assert!("Case #16: 4 8" == res);
}
