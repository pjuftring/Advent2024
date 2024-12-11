use std::collections::HashMap;

static MAX_STEPS: usize = 75;
type Cache = HashMap<usize, usize>;

fn convert(c: usize, n: usize) -> usize {
    assert!(0 < c && c <= MAX_STEPS);
    (c - 1) + n * MAX_STEPS
}

fn count(cache: &mut Cache, c: usize, n: usize) -> usize {
    if c == 0 {
        1
    } else if n == 0 {
        count(cache, c - 1, 1)
    } else {
        match cache.get(&convert(c, n)) {
            Some(m) => *m,
            None => {
                let s = n.to_string();
                if s.len() % 2 == 0 {
                    let (sl, sr) = s.split_at(s.len() / 2);
                    let resl = count(cache, c - 1, sl.parse().unwrap());
                    let resr = count(cache, c - 1, sr.parse().unwrap());
                    let res = resl + resr;
                    cache.insert(convert(c, n), res);
                    res
                } else {
                    count(cache, c - 1, n * 2024)
                }
            }
        }
    }
}

fn main() {
    let mut cache = HashMap::new();
    let nums = [5910927, 0, 1, 47, 261223, 94788, 545, 7771];
    for (c, text) in [(25, "First"), (75, "Second")] {
        let mut r: usize = 0;
        for n in nums {
            r += count(&mut cache, c, n);
        }
        println!("{} task: {}", text, r);
    }
}
