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
    println!("input = {}", input);
    do_work(input);
}


fn do_work(inp: &str) {
    let (port, chan) = SharedChan::new();
    let path = Path::new(inp);
    let mut file = BufferedReader::new(File::open(&path));
    let mut line_iterator = file.lines();

    let num_lines = line_iterator.next().unwrap();
    println!("1/num_lines = {}", num_lines);
    let num_lines: uint  = from_str(num_lines.trim()).unwrap();
    println!("2/num_lines = {}", num_lines);

    let mut i: uint = 0;

    while i < num_lines {
        println!("i = {}", i);
        let l1 = line_iterator.next().unwrap();
        println!("l1 = {}", l1);
        let l2 = line_iterator.next().unwrap();
        println!("l2 = {}", l2);
        let l3 = line_iterator.next().unwrap();
        println!("l3 = {}", l3);
        let my_chan = chan.clone();
        let i_is_mutable_and_cannot_be_passed = i;

        do spawn || {
            let result = ::calculate::find_items(i_is_mutable_and_cannot_be_passed, l1.trim(), l2.trim(), l3.trim());
            my_chan.send(result);
        }
        i += 1;
    }

    let mut results = port.iter();
    i = 0;
    while i < num_lines {
        let res = results.next().unwrap();
        println!("res = {}", res);
        i += 1;
    }
}


fn print_usage(program: &str, _opts: &[Opt]) {
    println!("Usage: {} [options]", program);
    println!("-f\t\tInput");
    println!("-h --help\tUsage");
}
