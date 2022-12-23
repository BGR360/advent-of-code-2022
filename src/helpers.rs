/*
 * Use this file if you want to extract helpers from your solutions.
 * Example import from this file: `use advent_of_code::helpers::example_fn;`.
 */

mod split_by;

pub use split_by::{SplitBy, SplitGroup, SplitGroups};

pub trait Itertools: Iterator {
    fn split_by<F>(self, predicate: F) -> SplitBy<Self, F>
    where
        F: FnMut(&Self::Item) -> bool,
        Self: Sized,
    {
        SplitBy {
            group_by: itertools::Itertools::group_by(self, predicate),
        }
    }
}

impl<I: Iterator> Itertools for I {}
