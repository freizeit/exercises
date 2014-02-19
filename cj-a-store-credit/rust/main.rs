/*
	A program solving

		http://code.google.com/codejam/contest/dashboard?c=351101#s=p0

	The test cases are contained in a file whose path is to be specified on
	the command line.

	Example:

		./main -f ../A-large-practice.in

	This will process the test cases contained in the 'A-large-practice.in'
*/


extern mod extra;
use extra::getopts::{reqopt,optflag,getopts,Opt};

use std::comm::SharedChan;
use std::io::File;
use std::io::buffered::BufferedReader;
use std::os;

mod calculate;


fn main() {
    let args = os::args();

    let program = args[0].clone();
    let opts = ~[
        reqopt("f"),
        optflag("h"),
        optflag("help")
    ];
    let matches = match getopts(args.tail(), opts) {
        Ok(m) => { m }
        Err(f) => {
            println(f.to_err_msg());
            print_usage(program, opts);
            return
        }
    };
    if matches.opt_present("h") || matches.opt_present("help") {
        print_usage(program, opts);
        return;
    }
    let input = matches.opt_str("f").unwrap();
    do_work(input);
}


fn do_work(input_file_path: &str) {
    let (port, chan) = SharedChan::new();

    let path = Path::new(input_file_path);
    let mut file = BufferedReader::new(File::open(&path));
    let mut line_iterator = file.lines();

    let num_tasks = line_iterator.next().unwrap();
    let num_tasks: uint  = from_str(num_tasks.trim()).unwrap();

    let mut i: uint = 0;

    while i < num_tasks {
        let l1 = line_iterator.next().unwrap();
        let l2 = line_iterator.next().unwrap();
        let l3 = line_iterator.next().unwrap();
        let my_chan = chan.clone();
        let i_is_mutable_and_cannot_be_passed = i + 1;

        do spawn || {
            let result = ::calculate::find_items(i_is_mutable_and_cannot_be_passed, l1.trim(), l2.trim(), l3.trim());
            my_chan.send(result);
        }
        i += 1;
    }

    let mut results = port.iter();
    i = 0;
    while i < num_tasks {
        let res = results.next().unwrap();
        println!("{}", res);
        i += 1;
    }
}


fn print_usage(program: &str, _opts: &[Opt]) {
    println!("Usage: {} [options]", program);
    println!("-f\t\tInput");
    println!("-h --help\tUsage");
}
