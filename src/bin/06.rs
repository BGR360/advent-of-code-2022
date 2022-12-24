#![doc = include_str!("../puzzles/06.md")]

use advent_of_code::debugln;

const START_OF_PACKET_LEN: usize = 4;
const START_OF_MESSAGE_LEN: usize = 14;

fn find_marker(msg: &[u8], marker_len: usize) -> usize {
    'outer: for (i, window) in msg.windows(marker_len).enumerate() {
        debugln!("i={i}, window={:?}", std::str::from_utf8(window).unwrap());
        let mut seen = [false; 256];
        for &c in window {
            if seen[c as usize] {
                continue 'outer;
            }
            seen[c as usize] = true;
        }

        return i + marker_len;
    }

    unreachable!()
}

pub fn part_one(input: &str) -> Option<usize> {
    Some(find_marker(input.as_bytes(), START_OF_PACKET_LEN))
}

pub fn part_two(input: &str) -> Option<usize> {
    Some(find_marker(input.as_bytes(), START_OF_MESSAGE_LEN))
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 6);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 6);
        assert_eq!(part_one(&input), Some(7));

        assert_eq!(part_one("bvwbjplbgvbhsrlpgdmjqwftvncz"), Some(5));
        assert_eq!(part_one("nppdvjthqldpwncqszvftbrmjlhg"), Some(6));
        assert_eq!(part_one("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), Some(10));
        assert_eq!(part_one("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), Some(11));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 6);
        assert_eq!(part_two(&input), Some(19));

        assert_eq!(part_two("bvwbjplbgvbhsrlpgdmjqwftvncz"), Some(23));
        assert_eq!(part_two("nppdvjthqldpwncqszvftbrmjlhg"), Some(23));
        assert_eq!(part_two("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), Some(29));
        assert_eq!(part_two("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), Some(26));
    }
}
