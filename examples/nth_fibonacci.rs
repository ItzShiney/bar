use bar::VM;

fn main() {
    let mut vm = VM::default();
    let input = r#"
        { nth-fibonacci(n) > a
            1 > a
            1 > b
            :loop
                le(n 0) > @if
                goif loop-end

                sum(a b) > b-new
                b > a
                b-new > b

                sub(n 1) > n
                go loop
            :loop-end
        }

        0 > n
        :loop
            print("F(" n ") = " nth-fibonacci(n))
            sum(n 1) > n

            le(n 20) > @if
            goif loop
    "#;

    if let Err(err) = vm.run(input) {
        println!("{}", err);
    }
}
