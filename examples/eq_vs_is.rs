use bar::VM;

fn main() {
    let mut vm = VM::default();
    let input = "
        1 > a
        1 > b
        print(eq(a b))
        print(is(a b))
    ";

    if let Err(err) = vm.run(input) {
        println!("{}", err);
    }
}
