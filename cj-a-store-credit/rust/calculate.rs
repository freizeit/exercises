/*
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	The test cases are contained in a file whose path is to be specified on
	the command line.

	Example:

		./main -f ../A-large-practice.in

	This will process the test cases contained in the 'A-large-practice.in'
*/


/*
 * This module contains the logic for solving the "store credit" problem.
 */


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
