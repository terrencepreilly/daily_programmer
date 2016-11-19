import 'dart:collection';
import 'dart:math';

const Map high_nums = const {
    10: 'a', 11: 'b', 12: 'c', 13: 'd', 14: 'e', 15: 'f',
    'a': 10, 'b': 11, 'c': 12, 'd': 13, 'e': 14, 'f': 15,
};

/* Takes a string or number, returns a number or string. */
Object transliterate(Object o) {
    if (high_nums.containsKey(o))
        return high_nums[o];
    else if (o is String)
        return int.parse(o);
    else if (o is int)
        return o.toString();
    return null;
}

int highest(String s) {
    return new SplayTreeSet.from(
        s.split('').map(transliterate)
    ).last;
}

int incr(int a) { return a + 1; }

int sum(List<int> l) { return l.reduce((a, b) => a + b); }

int min(int a, int b) { return a < b ? a : b; }

Iterable zip(Iterable a, Iterable b) {
    return new Iterable.generate(min(a.length, b.length),
        (i) => [a.elementAt(i), b.elementAt(i)]);
}

List<int> reverse(List<int> a) {
    return new List<int>.generate(a.length,
        (i) => a[(a.length-1) - i]);
}

List<int> repeat(int a, int n) {
    return new List<int>.generate(n, (x) => a);
}

List<int> iota(int a) {
    return new List<int>.generate(a, (i) => i);
}

int translate(int base, List<int> digits) {
    return sum(zip(reverse(digits),
        zip(repeat(base, digits.length), iota(digits.length))
            .map((x) => pow(x[0], x[1]))
    ).map((x) => x[0] * x[1]));
}

int translate_word(String w) {
    return translate(
        incr(highest(w)),
        new List<int>.generate(w.length, (i) => transliterate(w[i]))
    );
}

void main(List<String> args) {
    print(translate_word(args[0]));
}
