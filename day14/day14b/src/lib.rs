use num_integer::div_rem;

pub fn get_score_count_before(input: &str) -> usize {
    let digits: Vec<u8> = input
        .chars()
        .map(|c| c.to_digit(10).expect("Couldn't convert char to digit") as u8)
        .collect();
    let digits_len = digits.len();
    let mut scores = vec![3, 7];
    let mut elf1 = 0;
    let mut elf2 = 1;

    let mut at_end = false;
    let mut at_shifted_end = false;

    while !at_end && !at_shifted_end {
        add_new_scores(elf1, elf2, &mut scores);

        let (new_elf1, new_elf2) = get_new_elfs(elf1, elf2, &scores);
        elf1 = new_elf1;
        elf2 = new_elf2;

        let scores_len = scores.len();
        if scores_len > digits_len {
            at_end = scores[scores.len() - digits_len..].to_vec() == digits;
        }
        if scores_len > digits_len + 1 {
            at_shifted_end =
                scores[scores.len() - 1 - digits_len..scores.len() - 1].to_vec() == digits;
        }
    }

    if at_end {
        scores.len() - digits_len
    } else {
        scores.len() - digits_len - 1
    }
}

fn add_new_scores(elf1: usize, elf2: usize, scores: &mut Vec<u8>) {
    let (tens, ones) = div_rem(scores[elf1] + scores[elf2], 10);
    if tens > 0 {
        scores.push(tens);
    }
    scores.push(ones);
}

fn get_new_elfs(elf1: usize, elf2: usize, scores: &[u8]) -> (usize, usize) {
    let len = scores.len();

    let new_elf1 = (elf1 + scores[elf1] as usize + 1) % len;
    let new_elf2 = (elf2 + scores[elf2] as usize + 1) % len;

    (new_elf1, new_elf2)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_score_count_before() {
        assert_eq!(get_score_count_before("51589"), 9);
        assert_eq!(get_score_count_before("01245"), 5);
        assert_eq!(get_score_count_before("92510"), 18);
        assert_eq!(get_score_count_before("59414"), 2018);
    }

    #[test]
    fn test_add_new_scores() {
        let mut scores = vec![3, 7];

        add_new_scores(0, 1, &mut scores);
        assert_eq!(scores, vec![3, 7, 1, 0]);

        add_new_scores(0, 1, &mut scores);
        assert_eq!(scores, vec![3, 7, 1, 0, 1, 0]);

        add_new_scores(4, 3, &mut scores);
        assert_eq!(scores, vec![3, 7, 1, 0, 1, 0, 1]);
    }

    #[test]
    fn test_get_new_elfs() {
        assert_eq!(get_new_elfs(0, 1, &[3, 7, 1, 0]), (0, 1));
        assert_eq!(get_new_elfs(0, 1, &[3, 7, 1, 0, 1, 0]), (4, 3));
        assert_eq!(
            get_new_elfs(6, 3, &[3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1]),
            (8, 4)
        );
    }
}
