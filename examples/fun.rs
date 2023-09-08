use {
    bar::VM,
    nom::error::convert_error,
};

fn main() {
    let mut vm = VM::default();
    let input = "
        1 > a
        1 > b
        print(is(a b))
    ";

    if let Err(err) = vm.run(input) {
        println!("{}", convert_error(input, err));
    }
}
