use itertools::Itertools;

macro_rules! dbg {
    ($tt:tt) => {
        #[cfg(debug_assertions)]
        println!($tt)
    };
}

pub(super) struct MaxN<T> {
    n: usize,
    max_n: Vec<T>,
}

impl<T> MaxN<T> {
    #[inline]
    pub fn new(n: usize) -> Self {
        Self {
            n,
            max_n: Vec::new(),
        }
    }

    #[inline]
    pub fn get(self) -> Vec<T> {
        self.max_n
    }
}

impl<T> MaxN<T>
where
    T: Ord,
{
    #[inline]
    pub fn accumulate(&mut self, elt: T) {
        // The index where `elt` should go.
        let len = self.max_n.len();
        let insert_index = self.max_n.partition_point(|x| x >= &elt);

        if insert_index >= self.n {
            /* do nothing, we're full and element is too small */
            dbg!("Do nothing, we're full and element is too small");
        } else if len < self.n {
            dbg!("Insert at index {insert_index}");
            self.max_n.insert(insert_index, elt);
        } else {
            dbg!("Replace/shift at index {insert_index}");
            let subslice = &mut self.max_n[insert_index..len];
            subslice.rotate_right(1);
            subslice[0] = elt;
        }

        debug_assert!(self.invariants_hold());
    }

    fn invariants_hold(&self) -> bool {
        self.max_n.len() <= self.n && self.max_n.iter().tuples().all(|(a, b)| a >= b)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn do_test(
        input: impl IntoIterator<Item = u32>,
        n: usize,
        expected: impl IntoIterator<Item = u32>,
    ) {
        let mut max_n = MaxN::new(n);
        for i in input {
            max_n.accumulate(i);
        }
        assert_eq!(max_n.get(), expected.into_iter().collect::<Vec<_>>());
    }

    #[test]
    fn empty() {
        do_test([], 0, []);
        do_test([], 1, []);
        do_test([1, 2, 3], 0, []);
    }

    #[test]
    fn simple() {
        do_test([1, 2, 3], 1, [3]);
        do_test([1, 2, 3], 2, [3, 2]);
        do_test([1, 2, 3], 3, [3, 2, 1]);
    }

    #[test]
    fn reversed() {
        do_test([3, 2, 1], 1, [3]);
        do_test([3, 2, 1], 2, [3, 2]);
        do_test([3, 2, 1], 3, [3, 2, 1]);
    }

    #[test]
    fn replace() {
        do_test([1, 2, 3, 2], 4, [3, 2, 2, 1]);
        do_test([1, 2, 3, 2], 3, [3, 2, 2]);
        do_test([1, 2, 3, 2], 2, [3, 2]);
    }
}
