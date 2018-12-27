use num_integer::div_rem;

pub fn get_next_ten_scores(input: usize) -> Vec<u8> {
    let mut scores = vec![3, 7];
    let mut elf1 = 0;
    let mut elf2 = 1;

    while scores.len() < input + 10 {
        add_new_scores(elf1, elf2, &mut scores);

        let (new_elf1, new_elf2) = get_new_elfs(elf1, elf2, &scores);
        elf1 = new_elf1;
        elf2 = new_elf2;
    }

    scores[input..input + 10].to_vec()
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
    fn test_get_next_ten_scores() {
        assert_eq!(get_next_ten_scores(9), vec![5, 1, 5, 8, 9, 1, 6, 7, 7, 9]);
        assert_eq!(get_next_ten_scores(5), vec![0, 1, 2, 4, 5, 1, 5, 8, 9, 1]);
        assert_eq!(get_next_ten_scores(18), vec![9, 2, 5, 1, 0, 7, 1, 0, 8, 5]);
        assert_eq!(
            get_next_ten_scores(2018),
            vec![5, 9, 4, 1, 4, 2, 9, 8, 8, 2]
        );
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
        assert_eq!(get_new_elfs(0, 1, &vec![3, 7, 1, 0]), (0, 1));
        assert_eq!(get_new_elfs(0, 1, &vec![3, 7, 1, 0, 1, 0]), (4, 3));
        assert_eq!(
            get_new_elfs(6, 3, &vec![3, 7, 1, 0, 1, 0, 1, 2, 4, 5, 1]),
            (8, 4)
        );
    }
}
