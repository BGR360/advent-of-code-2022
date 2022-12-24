/*
 * Use this file if you want to extract helpers from your solutions.
 * Example import from this file: `use advent_of_code::helpers::example_fn;`.
 */

mod max_n;
pub mod parse;
mod split_by;

use max_n::MaxN;
pub use split_by::{SplitBy, SplitGroup, SplitGroups};

#[macro_export]
macro_rules! debug {
    ($($args:expr),*) => {{
        #[cfg(debug_assertions)]
        print!($($args),*);
    }};
}

#[macro_export]
macro_rules! debugln {
    ($($args:expr),*) => {{
        #[cfg(debug_assertions)]
        println!($($args),*);
    }};
}

pub trait Itertools: Iterator {
    /// Return an *iterable* that splits iterator elements into groups,
    /// separated by elements that satisfy a predicate.
    ///
    /// Elements that satisfy the predicate (“separators”) are not included in
    /// the split groups.
    ///
    /// `SplitBy` is the storage for the lazy grouping operation.
    ///
    /// If the groups are consumed in order, or if each group's iterator is
    /// dropped without keeping it around, then `SplitBy` uses no allocations.
    /// It needs allocations only if several group iterators are alive at the
    /// same time.
    ///
    /// This type implements [`IntoIterator`] (it is **not** an iterator
    /// itself), because the group iterators need to borrow from this value. It
    /// should be stored in a local variable or temporary and iterated.
    ///
    /// ```
    /// use advent_of_code::helpers::Itertools;
    ///
    /// // group data into runs separated by zeros.
    /// let data = vec![1, 3, 0, -2, 1, 0, 1, 2];
    /// // groups:     |--->|   |---->|   |--->|
    ///
    /// // Note: The `&` is significant here, `SplitBy` is iterable
    /// // only by reference. You can also call `.into_iter()` explicitly.
    /// let mut data_split: Vec<Vec<_>> = Vec::new();
    /// for group in &data.into_iter().split_by(|elt| *elt == 0) {
    ///     data_split.push(group.collect());
    /// }
    /// assert_eq!(data_split, vec![vec![1, 3], vec![-2, 1], vec![1, 2]]);
    /// ```
    fn split_by<F>(self, separator: F) -> SplitBy<Self, F>
    where
        F: FnMut(&Self::Item) -> bool,
        Self: Sized,
    {
        SplitBy {
            group_by: itertools::Itertools::group_by(self, separator),
        }
    }

    /// Returns the greatest N elements of an iterator.
    ///
    /// If the iterator is empty, an empty `Vec` is returned.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```
    /// use advent_of_code::helpers::Itertools;
    ///
    /// let a = [1, 2, 3, 4, 5];
    /// let b = vec![1, 2];
    /// let c: Vec<u32> = Vec::new();
    ///
    /// assert_eq!(a.into_iter().max_n(3), vec![5, 4, 3]);
    /// assert_eq!(b.into_iter().max_n(3), vec![2, 1]);
    /// assert_eq!(c.into_iter().max_n(3), vec![]);
    /// ```
    fn max_n(self, n: usize) -> Vec<Self::Item>
    where
        Self::Item: Ord,
        Self: Sized,
    {
        let max_n = self.fold(MaxN::new(n), |mut max_n, elt| {
            max_n.accumulate(elt);
            max_n
        });
        max_n.get()
    }
}

impl<I: Iterator> Itertools for I {}
