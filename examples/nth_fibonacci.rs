use bar::VM;

fn main() {
    let mut vm = VM::default();
    let input = r#"
        { nth-fibonacci(n) > a
            cp(n) > n
            
            1 > a
            1 > b
            :loop
                le(n 0) > @if
                goif loop-end

                sum(a b) > b-new
                b > a
                b-new > b

                dec(n)
                go loop
            :loop-end
        }

        0 > n
        :loop
            print("F(" n ") = " nth-fibonacci(n))
            inc(n)

            le(n 20) > @if
            goif loop
    "#;

    if let Err(err) = vm.run(input) {
        println!("{}", err);
    }
}
