func fib(_ n: Int) -> Int {
    fibIter(a: 1, b: 0, p: 0, q: 1, count: n)
}

func fibIter(a: Int, b: Int, p: Int, q: Int, count: Int) -> Int {
    if count == 0 {
        return b
    } else if count % 2 == 0 {
        return fibIter(a: a, b: b, p: p*p + q*q, q: q*q + 2*p*q, count: count/2)
    } else {
        return fibIter(a: b*q + a*q + a*p, b: b*p + a*q, p: p, q: q, count: count-1)
    }
}

// let clock = ContinuousClock()
// let result = clock.measure {
//     let n = 92
//     print("The \(n)th fibonacci number is \(fib(n))")
// }

// print("calculation took: \(result)")

protocol RationalNumber {
    var numerator: Int { get }
    var denominator: Int { get }

    static func makeRationalNumber(numerator: Int, denominator: Int) -> RationalNumber
}


struct RationalNumberImpl: RationalNumber {
    let numerator: Int
    let denominator: Int
    init(numerator: Int, denominator: Int) {
        self.numerator = numerator
        self.denominator = denominator
    }

    static func makeRationalNumber(numerator: Int, denominator: Int) -> RationalNumber {
        // do stuff to reduce
        RationalNumberImpl(numerator: numerator, denominator: denominator)
    }
}





func numerator(_ rationalNumber: RationalNumber) -> Int {
    rationalNumber.numerator
}

func denominator(_ rationalNumber: RationalNumber) -> Int {
    rationalNumber.denominator
}

let rational = RationalNumberImpl.makeRationalNumber(numerator: 5, denominator: 6)
print(rational.numerator)
print(rational.denominator)

let x: [String: any Equatable] = ["hi": 2]
