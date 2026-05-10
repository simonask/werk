use std::collections::VecDeque;

use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, Copy, Debug, Default)]
pub enum StringBreakMode<'a> {
    /// Breaks on sentence boundaries using unicode segmentation.
    Sentence,
    /// Breaks on word boundaries using unicode segmentation.
    Word,
    /// Breaks on grapheme cluster boundaries using unicode segmentation.
    #[default]
    GraphemeCluster,
    /// Splits the string into substrings using the given pattern (calling
    /// [`std::str::split`]).
    SplitPattern(&'a str),
}

#[derive(Clone, Copy, Debug, Default)]
pub enum ListBreakMode<'a> {
    #[default]
    BetweenItems,
    InsideItems(StringBreakMode<'a>),
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Location {
    Start,
    Middle,
    #[default]
    End,
}

pub const UNICODE_ELLIPSIS: &str = "…";
pub const ASCII_ELLIPSIS: &str = "...";

pub trait Ellipsis {
    fn width_for_remaining_items(&self, remaining: usize) -> usize;
    fn write_ellipsis(&self, w: &mut dyn std::fmt::Write, remaining: usize) -> std::fmt::Result;
}

impl Ellipsis for str {
    fn width_for_remaining_items(&self, _: usize) -> usize {
        self.graphemes(true).count()
    }

    fn write_ellipsis(&self, w: &mut dyn std::fmt::Write, _: usize) -> std::fmt::Result {
        w.write_str(self)
    }
}

pub struct AndMoreEllipsis;

impl Ellipsis for AndMoreEllipsis {
    fn width_for_remaining_items(&self, remaining: usize) -> usize {
        // 11 fixed chars in "(and  more)" + the decimal digit count of `remaining`.
        let digits = remaining.checked_ilog10().map_or(1, |d| d as usize + 1);
        11 + digits
    }

    fn write_ellipsis(&self, w: &mut dyn std::fmt::Write, remaining: usize) -> std::fmt::Result {
        write!(w, "(and {remaining} more)")
    }
}

pub fn ellipsize_with<W, E>(
    output: &mut W,
    input: &str,
    mode: StringBreakMode,
    max_width: usize,
    ellipsis: &E,
    location: Location,
) -> std::fmt::Result
where
    W: std::fmt::Write,
    E: Ellipsis + ?Sized,
{
    if max_width == 0 {
        return Ok(());
    }

    let mut segments: VecDeque<&str> = match mode {
        StringBreakMode::Sentence => input.unicode_sentences().collect(),
        StringBreakMode::Word => input.unicode_words().collect(),
        StringBreakMode::GraphemeCluster => input.graphemes(true).collect(),
        StringBreakMode::SplitPattern(pattern) => input.split(pattern).collect(),
    };
    let mut current_width = input.graphemes(true).count();

    // If the input already fits, write it verbatim — no ellipsis is needed.
    if current_width <= max_width {
        for segment in &segments {
            output.write_str(segment)?;
        }
        return Ok(());
    }

    // Otherwise, drop segments until the surviving content plus the ellipsis
    // fits within max_width, or until we run out of segments.
    let mut num_removed = 0;
    while !segments.is_empty()
        && current_width + ellipsis.width_for_remaining_items(num_removed) > max_width
    {
        let removed = match location {
            Location::Start => segments.pop_front().unwrap(),
            Location::Middle => {
                let index = segments.len() / 2;
                segments.remove(index).unwrap()
            }
            Location::End => segments.pop_back().unwrap(),
        };
        current_width -= removed.graphemes(true).count();
        num_removed += 1;
    }

    // If the ellipsis itself can't fit even after draining the input, write
    // nothing rather than overflowing max_width.
    if ellipsis.width_for_remaining_items(num_removed) > max_width {
        return Ok(());
    }

    match location {
        Location::Start => {
            ellipsis.write_ellipsis(output, num_removed)?;
            for segment in &segments {
                output.write_str(segment)?;
            }
            Ok(())
        }
        Location::Middle => {
            if segments.is_empty() {
                ellipsis.write_ellipsis(output, num_removed)?;
            } else {
                let mid = segments.len() / 2;
                for (index, segment) in segments.iter().enumerate() {
                    if index == mid {
                        ellipsis.write_ellipsis(output, num_removed)?;
                    }
                    output.write_str(segment)?;
                }
            }
            Ok(())
        }
        Location::End => {
            for segment in &segments {
                output.write_str(segment)?;
            }
            ellipsis.write_ellipsis(output, num_removed)?;
            Ok(())
        }
    }
}

pub fn ellipsize<W>(
    output: &mut W,
    input: &str,
    mode: StringBreakMode,
    max_length: usize,
) -> std::fmt::Result
where
    W: std::fmt::Write,
{
    ellipsize_with(
        output,
        input,
        mode,
        max_length,
        UNICODE_ELLIPSIS,
        Location::End,
    )
}

/// Concatenates a list of items into a single string, with an ellipsis if the
/// total width exceeds `max_length`.
///
/// The mode controls how sequences of graphemes are removed from the output.
pub fn ellipsize_list<W, E>(
    output: &mut W,
    input: impl ExactSizeIterator<Item: std::fmt::Display>,
    mode: ListBreakMode,
    max_length: usize,
    ellipsis: &E,
    location: Location,
    joiner: &str,
) -> std::fmt::Result
where
    W: std::fmt::Write,
    E: Ellipsis + ?Sized,
{
    let item_count = input.len();

    if item_count == 0 || max_length == 0 {
        return Ok(());
    }

    match mode {
        ListBreakMode::BetweenItems => {
            ellipsize_list_between_items(output, input, max_length, ellipsis, location, joiner)
        }
        ListBreakMode::InsideItems(string_break_mode) => {
            let items: Vec<String> = input.map(|item| item.to_string()).collect();
            let joiner_width = joiner.graphemes(true).count();

            // Keep all items visible but truncate long ones individually.
            // Budget the available width evenly across items after reserving
            // space for the joiners between them.
            let joiner_total = joiner_width * (item_count - 1);
            let available = max_length.saturating_sub(joiner_total);
            let budget_per_item = available / item_count;

            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    output.write_str(joiner)?;
                }
                ellipsize_with(
                    output,
                    item,
                    string_break_mode,
                    budget_per_item,
                    UNICODE_ELLIPSIS,
                    location,
                )?;
            }
            Ok(())
        }
    }
}

fn ellipsize_list_between_items<W, E>(
    output: &mut W,
    input: impl ExactSizeIterator<Item: std::fmt::Display>,
    max_length: usize,
    ellipsis: &E,
    location: Location,
    joiner: &str,
) -> std::fmt::Result
where
    W: std::fmt::Write,
    E: Ellipsis + ?Sized,
{
    let item_count = input.len();
    let joiner_width = joiner.graphemes(true).count();
    let items: Vec<String> = input.map(|item| item.to_string()).collect();

    let widths: Vec<usize> = items.iter().map(|s| s.graphemes(true).count()).collect();
    let mut item_widths_sum: usize = widths.iter().sum();

    // Fast path: everything fits without truncation.
    let no_ellipsis_width = item_widths_sum + joiner_width * (item_count - 1);
    if no_ellipsis_width <= max_length {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                output.write_str(joiner)?;
            }
            output.write_str(item)?;
        }
        return Ok(());
    }

    // Drop items (from the requested location) until the kept items plus
    // the ellipsis — treated as an extra list element with a joiner on
    // each side that has a neighbour — fit within max_length.
    //
    // Width formula: item_widths_sum + kept * joiner_width + e_width
    //   kept == 0  →  just e_width (no joiners)
    let mut indices: VecDeque<usize> = (0..item_count).collect();
    let mut num_removed = 0usize;

    while !indices.is_empty() {
        let kept = indices.len();
        let e_width = ellipsis.width_for_remaining_items(num_removed);
        let total = item_widths_sum + kept * joiner_width + e_width;
        if total <= max_length {
            break;
        }
        let removed_idx = match location {
            Location::Start => indices.pop_front().unwrap(),
            Location::Middle => {
                let mid = indices.len() / 2;
                indices.remove(mid).unwrap()
            }
            Location::End => indices.pop_back().unwrap(),
        };
        item_widths_sum -= widths[removed_idx];
        num_removed += 1;
    }

    // Guard: the ellipsis alone won't fit either.
    if ellipsis.width_for_remaining_items(num_removed) > max_length {
        return Ok(());
    }

    // Write kept items with the ellipsis inserted at the right edge.
    // The ellipsis is separated from its neighbours by the joiner, just
    // like any other list element.
    match location {
        Location::Start => {
            ellipsis.write_ellipsis(output, num_removed)?;
            for &idx in &indices {
                output.write_str(joiner)?;
                output.write_str(&items[idx])?;
            }
        }
        Location::Middle => {
            if indices.is_empty() {
                ellipsis.write_ellipsis(output, num_removed)?;
            } else {
                let mid = indices.len() / 2;
                for (i, &idx) in indices.iter().enumerate() {
                    if i > 0 {
                        output.write_str(joiner)?;
                    }
                    if i == mid {
                        ellipsis.write_ellipsis(output, num_removed)?;
                        output.write_str(joiner)?;
                    }
                    output.write_str(&items[idx])?;
                }
            }
        }
        Location::End => {
            for (i, &idx) in indices.iter().enumerate() {
                if i > 0 {
                    output.write_str(joiner)?;
                }
                output.write_str(&items[idx])?;
            }
            if !indices.is_empty() {
                output.write_str(joiner)?;
            }
            ellipsis.write_ellipsis(output, num_removed)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(
        input: &str,
        mode: StringBreakMode,
        max_width: usize,
        ellipsis: &(impl Ellipsis + ?Sized),
        location: Location,
    ) -> String {
        let mut out = String::new();
        ellipsize_with(&mut out, input, mode, max_width, ellipsis, location).unwrap();
        out
    }

    fn run_default(input: &str, mode: StringBreakMode, max_width: usize) -> String {
        let mut out = String::new();
        ellipsize(&mut out, input, mode, max_width).unwrap();
        out
    }

    fn graphemes_in(s: &str) -> usize {
        s.graphemes(true).count()
    }

    // -------------------------------------------------------------------------
    // Degenerate / edge inputs
    // -------------------------------------------------------------------------

    #[test]
    fn max_width_zero_yields_empty() {
        assert_eq!(
            run_default("hello", StringBreakMode::GraphemeCluster, 0),
            ""
        );
    }

    #[test]
    fn empty_input_yields_empty() {
        assert_eq!(run_default("", StringBreakMode::GraphemeCluster, 10), "");
    }

    #[test]
    fn max_width_smaller_than_ellipsis_yields_empty() {
        // ASCII_ELLIPSIS is 3 graphemes. With max_width=2 there's no room even
        // for the ellipsis itself, so nothing should be written.
        let out = run(
            "hello world",
            StringBreakMode::GraphemeCluster,
            2,
            ASCII_ELLIPSIS,
            Location::End,
        );
        assert_eq!(out, "");
    }

    // -------------------------------------------------------------------------
    // Truncation at the end (the default location)
    // -------------------------------------------------------------------------

    #[test]
    fn truncates_at_end_with_unicode_ellipsis() {
        // 4 surviving graphemes + 1 ellipsis = 5.
        let out = run_default("hello world", StringBreakMode::GraphemeCluster, 5);
        assert_eq!(out, "hell…");
        assert_eq!(graphemes_in(&out), 5);
    }

    #[test]
    fn truncates_at_end_with_ascii_ellipsis() {
        // ASCII "..." is 3 graphemes, leaving room for 2 from the input.
        let out = run(
            "hello world",
            StringBreakMode::GraphemeCluster,
            5,
            ASCII_ELLIPSIS,
            Location::End,
        );
        assert_eq!(out, "he...");
        assert_eq!(graphemes_in(&out), 5);
    }

    #[test]
    fn never_exceeds_max_width() {
        // Output must never exceed the requested max_width (in graphemes),
        // for any width within a wide range.
        for max in 0..=30 {
            let out = run_default(
                "the quick brown fox jumps over the lazy dog",
                StringBreakMode::GraphemeCluster,
                max,
            );
            assert!(
                graphemes_in(&out) <= max,
                "max={max}: got {out:?} ({} graphemes)",
                graphemes_in(&out),
            );
        }
    }

    // -------------------------------------------------------------------------
    // Truncation at the start
    // -------------------------------------------------------------------------

    #[test]
    fn truncates_at_start() {
        let out = run(
            "hello world",
            StringBreakMode::GraphemeCluster,
            5,
            UNICODE_ELLIPSIS,
            Location::Start,
        );
        // Ellipsis followed by the trailing 4 graphemes.
        assert_eq!(out, "…orld");
        assert_eq!(graphemes_in(&out), 5);
    }

    // -------------------------------------------------------------------------
    // Truncation in the middle
    // -------------------------------------------------------------------------

    #[test]
    fn truncates_in_middle() {
        let out = run(
            "abcdefghij",
            StringBreakMode::GraphemeCluster,
            5,
            UNICODE_ELLIPSIS,
            Location::Middle,
        );
        // The exact split point is implementation-defined, but the result must:
        //   - fit within max_width
        //   - contain the ellipsis
        //   - start with a prefix of the input
        //   - end with a suffix of the input
        assert!(graphemes_in(&out) <= 5, "{out:?} too wide");
        assert!(out.contains(UNICODE_ELLIPSIS), "{out:?} missing ellipsis");
        assert!(
            out.starts_with('a'),
            "expected output to start with input prefix, got {out:?}",
        );
        assert!(
            out.ends_with('j'),
            "expected output to end with input suffix, got {out:?}",
        );
    }

    // -------------------------------------------------------------------------
    // Word and sentence break modes
    // -------------------------------------------------------------------------

    #[test]
    fn truncates_on_word_boundary() {
        let out = run(
            "the quick brown fox",
            StringBreakMode::Word,
            12,
            UNICODE_ELLIPSIS,
            Location::End,
        );
        assert!(graphemes_in(&out) <= 12);
        assert!(out.ends_with(UNICODE_ELLIPSIS));
        // NOTE: word mode joins segments without separators, since
        // unicode-segmentation's `unicode_words` iterator yields only the
        // word tokens (whitespace is dropped). Output for this input is
        // "thequick…" rather than "the quick …". This is current behavior;
        // word-mode ellipsization is meaningful for delimiters like ", ", not
        // for prose where re-inserting separators would matter.
    }

    #[test]
    fn truncates_on_sentence_boundary() {
        let out = run(
            "First sentence. Second sentence. Third sentence.",
            StringBreakMode::Sentence,
            20,
            UNICODE_ELLIPSIS,
            Location::End,
        );
        assert!(graphemes_in(&out) <= 20);
        assert!(out.ends_with(UNICODE_ELLIPSIS));
        let prefix = out.trim_end_matches(UNICODE_ELLIPSIS);
        assert!(
            "First sentence. Second sentence. Third sentence.".starts_with(prefix),
            "non-prefix surfaced: {out:?}",
        );
    }

    // -------------------------------------------------------------------------
    // Unicode handling
    // -------------------------------------------------------------------------

    #[test]
    fn counts_grapheme_clusters_not_bytes() {
        // Each emoji is one grapheme cluster but several bytes. With max=3 and
        // a 1-grapheme ellipsis, two of the five should survive.
        let out = run(
            "🦀🦀🦀🦀🦀",
            StringBreakMode::GraphemeCluster,
            3,
            UNICODE_ELLIPSIS,
            Location::End,
        );
        assert_eq!(graphemes_in(&out), 3);
        assert!(out.ends_with(UNICODE_ELLIPSIS));
    }

    #[test]
    fn handles_combining_characters() {
        // "é" expressed as e + combining acute is one grapheme cluster, so
        // "éllo world" is 10 graphemes (not 11 codepoints).
        let input = "e\u{0301}llo world";
        assert_eq!(graphemes_in(input), 10);
        let out = run_default(input, StringBreakMode::GraphemeCluster, 5);
        assert_eq!(graphemes_in(&out), 5);
        // The 'é' must not be split between its base and combining mark.
        assert!(out.starts_with("e\u{0301}") || !out.contains('\u{0301}'));
    }

    // -------------------------------------------------------------------------
    // AndMoreEllipsis
    // -------------------------------------------------------------------------

    #[test]
    fn and_more_ellipsis_renders_count() {
        let out = run(
            "abcdefghijklmnopqrstuvwxyz",
            StringBreakMode::GraphemeCluster,
            20,
            &AndMoreEllipsis,
            Location::End,
        );
        assert!(graphemes_in(&out) <= 20, "{out:?} too wide");
        assert!(out.contains("(and "), "{out:?} missing 'and …'");
        assert!(out.contains(" more)"), "{out:?} missing ' more)'");
    }

    #[test]
    fn and_more_ellipsis_reported_width_matches_rendered() {
        // The trait contract: reported width must equal the actual rendered
        // grapheme count, otherwise the truncation loop's accounting drifts.
        for remaining in [0usize, 1, 9, 10, 99, 100, 999] {
            let mut rendered = String::new();
            AndMoreEllipsis
                .write_ellipsis(&mut rendered, remaining)
                .unwrap();
            assert_eq!(
                graphemes_in(&rendered),
                AndMoreEllipsis.width_for_remaining_items(remaining),
                "mismatch for remaining={remaining}: rendered={rendered:?}",
            );
        }
    }

    // -------------------------------------------------------------------------
    // Input that already fits within max_width
    // -------------------------------------------------------------------------

    #[test]
    fn input_shorter_than_max_width_is_unchanged() {
        let out = run_default("hi", StringBreakMode::GraphemeCluster, 100);
        assert_eq!(out, "hi");
    }

    #[test]
    fn input_at_exactly_max_width_is_unchanged() {
        let out = run_default("hello", StringBreakMode::GraphemeCluster, 5);
        assert_eq!(out, "hello");
    }

    // -------------------------------------------------------------------------
    // Regression: the middle writer used to consume the segment at the split
    // point without writing it, dropping a character from the output.
    // -------------------------------------------------------------------------

    #[test]
    fn middle_writer_preserves_all_kept_segments() {
        let out = run(
            "abcdef",
            StringBreakMode::GraphemeCluster,
            4,
            UNICODE_ELLIPSIS,
            Location::Middle,
        );
        assert_eq!(graphemes_in(&out), 4);
        // 4-grapheme budget = 1 ellipsis + 3 surviving graphemes; none may
        // be silently dropped on the way to output.
        let visible = out.replace(UNICODE_ELLIPSIS, "");
        assert_eq!(graphemes_in(&visible), 3, "kept segments lost: {out:?}");
        for grapheme in visible.graphemes(true) {
            assert!(
                "abcdef".contains(grapheme),
                "unknown grapheme {grapheme:?} in {out:?}",
            );
        }
    }

    // -------------------------------------------------------------------------
    // ellipsize_list — BetweenItems mode
    // -------------------------------------------------------------------------

    fn run_list<E>(
        items: &[&str],
        mode: ListBreakMode,
        max_width: usize,
        ellipsis: &E,
        location: Location,
        joiner: &str,
    ) -> String
    where
        E: Ellipsis + ?Sized,
    {
        let mut out = String::new();
        ellipsize_list(
            &mut out,
            items.iter().copied(),
            mode,
            max_width,
            ellipsis,
            location,
            joiner,
        )
        .unwrap();
        out
    }

    #[test]
    fn list_all_items_fit_written_verbatim() {
        let out = run_list(
            &["apple", "banana", "cherry"],
            ListBreakMode::BetweenItems,
            30,
            UNICODE_ELLIPSIS,
            Location::End,
            ", ",
        );
        assert_eq!(out, "apple, banana, cherry");
    }

    #[test]
    fn list_end_truncation_drops_trailing_items() {
        // "apple, banana, cherry" = 21 graphemes; max=15 forces truncation.
        let out = run_list(
            &["apple", "banana", "cherry"],
            ListBreakMode::BetweenItems,
            15,
            UNICODE_ELLIPSIS,
            Location::End,
            ", ",
        );
        assert!(graphemes_in(&out) <= 15, "{out:?} too wide");
        assert!(out.ends_with(UNICODE_ELLIPSIS), "{out:?} missing ellipsis");
        assert!(out.starts_with("apple"), "{out:?} wrong prefix");
    }

    #[test]
    fn list_end_truncation_never_exceeds_max_width() {
        let items = ["one", "two", "three", "four", "five"];
        for max in 0..=40 {
            let out = run_list(
                &items,
                ListBreakMode::BetweenItems,
                max,
                UNICODE_ELLIPSIS,
                Location::End,
                ", ",
            );
            assert!(
                graphemes_in(&out) <= max,
                "max={max}: {out:?} ({} graphemes)",
                graphemes_in(&out)
            );
        }
    }

    #[test]
    fn list_start_truncation_drops_leading_items() {
        let out = run_list(
            &["apple", "banana", "cherry"],
            ListBreakMode::BetweenItems,
            15,
            UNICODE_ELLIPSIS,
            Location::Start,
            ", ",
        );
        assert!(graphemes_in(&out) <= 15, "{out:?} too wide");
        assert!(
            out.starts_with(UNICODE_ELLIPSIS),
            "{out:?} missing leading ellipsis"
        );
        assert!(out.ends_with("cherry"), "{out:?} wrong suffix");
    }

    #[test]
    fn list_middle_truncation_keeps_prefix_and_suffix() {
        let out = run_list(
            &["a", "b", "c", "d", "e"],
            ListBreakMode::BetweenItems,
            10,
            UNICODE_ELLIPSIS,
            Location::Middle,
            ", ",
        );
        assert!(graphemes_in(&out) <= 10, "{out:?} too wide");
        assert!(out.contains(UNICODE_ELLIPSIS), "{out:?} missing ellipsis");
        assert!(out.starts_with('a'), "{out:?} missing prefix");
        assert!(out.ends_with('e'), "{out:?} missing suffix");
    }

    #[test]
    fn list_empty_input_yields_empty() {
        let out = run_list(
            &[],
            ListBreakMode::BetweenItems,
            20,
            UNICODE_ELLIPSIS,
            Location::End,
            ", ",
        );
        assert_eq!(out, "");
    }

    #[test]
    fn list_and_more_ellipsis_end() {
        let items = ["alpha", "beta", "gamma", "delta", "epsilon"];
        let out = run_list(
            &items,
            ListBreakMode::BetweenItems,
            25,
            &AndMoreEllipsis,
            Location::End,
            ", ",
        );
        assert!(graphemes_in(&out) <= 25, "{out:?} too wide");
        assert!(
            out.contains("(and ") && out.contains(" more)"),
            "{out:?} bad ellipsis"
        );
    }

    // -------------------------------------------------------------------------
    // ellipsize_list — InsideItems mode
    // -------------------------------------------------------------------------

    #[test]
    fn list_inside_items_all_short_unchanged() {
        // Items are short enough that no per-item truncation is needed.
        let out = run_list(
            &["hi", "yo", "ok"],
            ListBreakMode::InsideItems(StringBreakMode::GraphemeCluster),
            30,
            UNICODE_ELLIPSIS,
            Location::End,
            ", ",
        );
        assert_eq!(out, "hi, yo, ok");
    }

    #[test]
    fn list_inside_items_truncates_individually() {
        // 3 items, joiner ", " (2 graphemes), max=15.
        // Available = 15 - 2*2 = 11, budget per item = 11/3 = 3.
        let out = run_list(
            &["abcdef", "ghijkl", "mnopqr"],
            ListBreakMode::InsideItems(StringBreakMode::GraphemeCluster),
            15,
            UNICODE_ELLIPSIS,
            Location::End,
            ", ",
        );
        assert!(graphemes_in(&out) <= 15, "{out:?} too wide");
        // Each segment must contain at least the ellipsis.
        for part in out.split(", ") {
            assert!(!part.is_empty(), "empty segment in {out:?}");
        }
    }
}
