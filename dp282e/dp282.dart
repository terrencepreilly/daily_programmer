import 'dart:collection';

import '../dp281e/dp281e.dart';

/* end-to-end join */
List join(List a, List b) => []..addAll(a)..addAll(b);

List<int> fib(int i, [List<int> nums=const[]]) {
    if (i == 0)
        return nums;
    else if (nums.length < 2)
        return fib(i-1, join([1], nums));
    else
        return fib(i-1, join([nums[0] + nums[1]], nums));
}

int parse_line(String line) {
    int parse(String a, String b) {
        return translate(
            int.parse(a),
            new List<int>.generate(
                b.length,
                (i) => transliterate(b[i])
                )
            );
    }
    return Function.apply(parse, line.split(' '));
}

int max_less_than_index(int m, List<int> l) {
    int max_lt([int i = 0]) {
        if (i > l.length)
            return -1;
        else if (l[i] <= m)
            return i;
        return max_lt(i+1);
    }
    return max_lt();
}


int max_less_than(int m, List<int> l) => l[max_less_than_index(m, l)];

List<int> piecewise_or(List<int> a, List<int> b) {
    return new List<int>.generate(min(a.length, b.length),
        (i) { return a[i] == 1 || b[i] == 1
                     ? 1
                     : 0; });
}

List<int> zeros_except(int n, int i) {
    return join(join(new List<int>.generate(i, (j) => 0), [1]),
                new List<int>.generate(n-i-1, (j) => 0));
}

List<int> to_fib(List<int> fibseq, int i, [List<List<int>> l=const[]]) {
    List<int> zero_list() =>
        zeros_except(fibseq.length, max_less_than_index(i, fibseq));
    if (i == 0)
        return l.reduce(piecewise_or);
    return to_fib(
        fibseq,
        i - max_less_than(i, fibseq),
        l.length == 0 ? [zero_list()] : join(l, [zero_list()])
        );
}

List<int> trim_leading_zeros(List<int> l) {
    if (l[0] == 1)
        return l;
    return trim_leading_zeros(
        new List<int>.generate(l.length - 1, (i) => l[i+1])
        );
}

void print_list(List<int> l) {
    print(l.map((x) => x.toString()).join(''));
}

int from_fib(List<int> l) =>
    sum(zip(l, fib(l.length)).map((x) => x[0] * x[1]));

main(List<String> args) {
    if (args[0][0] == 'F')
        print(
            from_fib(
                args[0].split(' ')[1].split('').map((x) => int.parse(x))
            )
        );
    else
        print_list(
            trim_leading_zeros(to_fib(fib(15), parse_line(args[0])))
        );
}
