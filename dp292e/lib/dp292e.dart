    RegExp RANGE_DELIMS = new RegExp('-|:|\\.\\.');


    class Digit {
        String repr;

        Digit(var x) {
            this.repr = x.toString();
        }

        int parse() => int.parse(this.repr);

        int length() => this.repr.length;

        String sub(Digit d) {
            if (this.length() > d.length())
                return this.repr.substring(0, this.length() - d.length());
            return '1';
        }

        int force_up(Digit prior) {
            int a = prior.parse();
            int b = this.parse();
            String sigs = prior.sub(this);
            Function incr = (s) => (int.parse(s) + 1).toString();
            if (a < b)
                return b;
            else if (a == b)
                return int.parse(sigs + this.repr);
            else
                return int.parse(sigs + this.repr) > a
                    ? int.parse(sigs + this.repr)
                    : int.parse(incr(sigs) + this.repr);
        }
    }


    class Range {
        String lower;
        String upper;
        int step;

        Range(String s) {
            List<String> arr = s.split(RANGE_DELIMS);
            lower = arr[0];
            upper = arr[1];
            step = arr.length > 2
                ? int.parse(arr[2])
                : 1;
        }

        Iterable<int> force_up(Digit prior) sync* {
            int l = new Digit(this.lower).force_up(prior);
            int u = new Digit(this.upper)
                .force_up(new Digit(l.toString()));
            for (; l <= u; l += this.step)
                yield l;
        }
    }


    List<int> expand(String s) {
        List<int> ret = new List<int>();
        Digit prior = new Digit('0');
        for (String x in s.split(',')) {
            if (x.contains(RANGE_DELIMS)) {
                Range r = new Range(x);
                for (int a in r.force_up(prior))
                    ret.add(a);
                prior = new Digit(ret[ret.length - 1]);
            } else {
                Digit d = new Digit(x);
                int a = d.force_up(prior);
                ret.add(a);
                prior = new Digit(a);
            }
        }
        return ret;
    }


    void main(List<String> args) {
        List<String> ss = ["1,3,7,2,4,1", "1-3,1-2", "1:5:2",
                           "104-2", "104..02", "545,64:11"];
        for (String s in ss)
            print(expand(s));
    }
