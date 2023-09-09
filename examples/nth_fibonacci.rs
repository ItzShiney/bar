use bar::VM;

fn main() {
    let mut vm = VM::default();
    let input = r#"
        { nth-fibonacci(n) > a
            cp(n) > n
            
            1 > a
            1 > b
            tag loop
                le(n 0) > @if
                goif loop-end

                sum(a b) > b-new
                b > a
                b-new > b

                dec(n)
                go loop
            tag loop-end
        }

        0 > n
        tag loop
            print("F(" n ") = " nth-fibonacci(n))
            inc(n)

            le(n 20) > @if
            goif loop
    "#;

    vm.run(input).unwrap();
}
