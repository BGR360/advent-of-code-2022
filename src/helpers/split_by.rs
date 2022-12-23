/// `SplitBy` is the storage for the lazy splitting operation.
///
/// This type implements [`IntoIterator`] (it is **not** an iterator itself),
/// because the group iterators need to borrow from this value. It should be
/// stored in a local variable or temporary and iterated.
pub struct SplitBy<I, F>
where
    I: Iterator,
{
    pub(super) group_by: itertools::GroupBy<bool, I, F>,
}

impl<'a, I, F> IntoIterator for &'a SplitBy<I, F>
where
    I: Iterator,
    F: FnMut(&I::Item) -> bool,
{
    type Item = SplitGroup<'a, I, F>;

    type IntoIter = SplitGroups<'a, I, F>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        let groups = (&self.group_by).into_iter();
        SplitGroups::new(groups)
    }
}

pub struct SplitGroups<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
{
    groups: itertools::Groups<'a, bool, I, F>,
    /// `Some` if currently iterating through a run of separators.
    separator_group: Option<itertools::Group<'a, bool, I, F>>,
    /// `true` if `next()` has not been called yet.
    is_first: bool,
}

impl<'a, I, F> SplitGroups<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
{
    #[inline]
    fn new(groups: itertools::Groups<'a, bool, I, F>) -> Self {
        Self {
            groups,
            separator_group: None,
            is_first: true,
        }
    }
}

impl<'a, I, F> Iterator for SplitGroups<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
    F: FnMut(&I::Item) -> bool,
{
    type Item = SplitGroup<'a, I, F>;

    fn next(&mut self) -> Option<Self::Item> {
        let is_first_iter = self.is_first;
        self.is_first = false;
        loop {
            // Yield empty groups for each repeat separator
            if let Some(mut separator_group) = self.separator_group.take() {
                if separator_group.next().is_some() {
                    self.separator_group = Some(separator_group);
                    return Some(SplitGroup::empty());
                }
            }

            // See if the next group is a run of separators or non-separators.
            let Some((is_separator, mut group)) = self.groups.next() else {
                return None;
            };

            if !is_separator {
                return Some(SplitGroup::new(group));
            }

            // Ignore the first separator in the run.
            group.next();

            debug_assert!(self.separator_group.is_none());
            self.separator_group = Some(group);

            // If the separator is the first thing, yield an empty group.
            if is_first_iter {
                return Some(SplitGroup::empty());
            }
        }
    }
}

pub struct SplitGroup<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
{
    /// `None` if this is an empty group
    group: Option<itertools::Group<'a, bool, I, F>>,
}

impl<'a, I, F> SplitGroup<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
{
    #[inline]
    fn new(group: itertools::Group<'a, bool, I, F>) -> Self {
        Self { group: Some(group) }
    }

    #[inline]
    fn empty() -> Self {
        Self { group: None }
    }
}

impl<'a, I, F> Iterator for SplitGroup<'a, I, F>
where
    I: Iterator,
    I::Item: 'a,
    F: FnMut(&I::Item) -> bool,
{
    type Item = I::Item;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.group.as_mut().and_then(|group| group.next())
    }
}

#[cfg(test)]
mod tests {
    use super::super::Itertools;

    #[track_caller]
    fn run_split_by_char_test<'a>(
        input: &str,
        separator: char,
        expected: impl IntoIterator<Item = &'a str>,
    ) {
        let expected: Vec<String> = expected.into_iter().map(|s| s.to_owned()).collect();

        // Sanity check: compare to std::str::split
        let std_expected: Vec<String> = input
            .split_terminator(separator)
            .map(|s| s.to_owned())
            .collect();
        assert_eq!(
            std_expected, expected,
            "Expected behavior does not match `std`"
        );

        let split_by = input.chars().split_by(|&ch| ch == separator);

        let actual: Vec<String> = (&split_by)
            .into_iter()
            .map(|chars| chars.collect())
            .collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn empty() {
        run_split_by_char_test("", ' ', vec![]);
    }

    #[test]
    fn simple() {
        run_split_by_char_test("a b", ' ', vec!["a", "b"]);
        run_split_by_char_test("a b c", ' ', vec!["a", "b", "c"]);
        run_split_by_char_test("abc de f", ' ', vec!["abc", "de", "f"]);
    }

    #[test]
    fn separator_at_endpoints() {
        run_split_by_char_test(" a", ' ', vec!["", "a"]);
        run_split_by_char_test("a ", ' ', vec!["a"]);
        run_split_by_char_test(" a ", ' ', vec!["", "a"]);
    }

    #[test]
    fn only_separator() {
        run_split_by_char_test(" ", ' ', vec![""]);
    }

    #[test]
    fn repeated_separator() {
        run_split_by_char_test("a  b", ' ', vec!["a", "", "b"]);
    }
}
