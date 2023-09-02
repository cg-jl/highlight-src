use std::{fmt::Display, num::NonZeroUsize, ops::Range};

use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Config {
    /// The file to look at.
    file: std::path::PathBuf,

    /// The begin offset
    begin_offset: usize,

    /// The end offset
    end_offset: Option<NonZeroUsize>,

    /// Whether the begin and end offsets act instead as line/column numbers
    #[structopt(long = "reverse")]
    reverse: bool,

    /// Context lines
    #[structopt(short = "C")]
    context_lines: Option<usize>,
}

struct HighlightFormatter<'f, 'a> {
    inner_std: &'f mut std::fmt::Formatter<'a>,
    highlight_range: Range<usize>,
}

struct UseHighlight<'a, T> {
    value: &'a T,
    highlight_range: Range<usize>,
}

impl<T: DisplayWithHighlight> Display for UseHighlight<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut fmt = HighlightFormatter {
            inner_std: f,
            highlight_range: self.highlight_range.clone(),
        };

        self.value.fmt_with_highlight(&mut fmt)
    }
}

impl<T: Display> DisplayWithHighlight for T {
    fn fmt_with_highlight(&self, f: &mut HighlightFormatter) -> std::fmt::Result {
        self.fmt(f.inner_std)
    }
}

impl std::fmt::Write for HighlightFormatter<'_, '_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.inner_std.write_str(s)
    }
}

impl HighlightFormatter<'_, '_> {
    const HL_COLOR: &'static str = "\x1b[38;5;255m";
    const RESET: &'static str = "\x1b[m";
    /// Gets an `str` and the range the slice refers to, so it can be compared
    /// with the highlight range
    fn write_with_highlight(&mut self, s: &str, fit_range: &Range<usize>) -> std::fmt::Result {
        if let Some(hl) = self.get_highlighted_range(fit_range) {
            self.inner_std.write_str(&s[fit_range.start..hl.start])?;
            // this portion is highlighted
            self.do_highlighted(|f| f.write_str(&s[hl.clone()]))?;
            self.inner_std.write_str(&s[hl.end..fit_range.end])
        } else {
            // invalid range: the portion is not highlighted
            self.inner_std.write_str(&s[fit_range.clone()])
        }
    }

    fn get_highlighted_range(&self, fit_range: &Range<usize>) -> Option<Range<usize>> {
        // ensure the highlight range is in bounds
        let hl_start = fit_range.start.max(self.highlight_range.start);
        let hl_end = fit_range.end.min(self.highlight_range.end);
        if hl_start >= hl_end {
            None
        } else {
            Some(hl_start..hl_end)
        }
    }

    fn do_highlighted(
        &mut self,
        cont: impl FnOnce(&mut Self) -> std::fmt::Result,
    ) -> std::fmt::Result {
        self.inner_std.write_str(Self::HL_COLOR)?;
        cont(self)?;
        self.inner_std.write_str(Self::RESET)
    }

    #[inline]
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.inner_std.write_str(s)
    }
}

enum Line<'src> {
    Complete {
        source: &'src str,
        range: Range<usize>,
    },
    Cut {
        source: &'src str,
        line_range: Range<usize>,
        start_is_line_start: bool,
        end_is_line_end: bool,
    },
}

trait DisplayWithHighlight {
    fn fmt_with_highlight(&self, f: &mut HighlightFormatter) -> std::fmt::Result;
}

impl DisplayWithHighlight for Line<'_> {
    fn fmt_with_highlight(&self, f: &mut HighlightFormatter) -> std::fmt::Result {
        match self {
            Self::Complete { source, range } => f.write_with_highlight(source, range),
            Self::Cut {
                source,
                start_is_line_start,
                end_is_line_end,
                line_range,
            } => {
                if !*start_is_line_start {
                    f.write_str("...")?;
                }
                f.write_with_highlight(source, line_range)?;
                if !*end_is_line_end {
                    f.write_str("...")?;
                }
                Ok(())
            }
        }
    }
}

impl<'src> Line<'src> {
    pub fn new(
        full_source: &'src str,
        line_range: Range<usize>,
        desired_range_if_cut: &Range<usize>,
    ) -> Self {
        if line_range.len() <= 72 {
            Self::Complete {
                range: line_range,
                source: full_source,
            }
        } else {
            // add 10 characters of context per side
            const CONTEXT: usize = 10;
            let cut_range = desired_range_if_cut
                .start
                .saturating_sub(CONTEXT)
                .max(line_range.start)
                ..line_range
                    .end
                    .min(desired_range_if_cut.end.saturating_add(CONTEXT))
                    .min(line_range.start + 72);

            Self::Cut {
                end_is_line_end: cut_range.end == line_range.end,
                start_is_line_start: cut_range.start == line_range.start,
                line_range: cut_range,
                source: full_source,
            }
        }
    }
}

struct LinePosInfo {
    line_no: usize,
    col_no: usize,
    line_range: Range<usize>,
}

impl LinePosInfo {
    pub fn from_offset(input: &str, offset: usize) -> Self {
        let mut line_start = 0;
        let mut col_no = 1;
        let mut line_no = 1;

        for (ch_offset, ch) in input.char_indices() {
            if ch == '\n' {
                if ch_offset >= offset {
                    return Self {
                        line_no,
                        col_no,
                        line_range: line_start..ch_offset,
                    };
                }

                line_start = ch_offset + ch.len_utf8();
                col_no = 1;
                line_no += 1;
            } else {
                col_no += 1;
            }
        }

        Self {
            line_no,
            col_no,
            line_range: line_start..input.len(),
        }
    }
}

struct SourceInfo<'src> {
    source: &'src str,
    context_lines: usize,
    line_start: Line<'src>,
    line_end: Line<'src>,
    line_start_info: LinePosInfo,
    line_end_info: LinePosInfo,
    full_range: Range<usize>,
}

impl<'src> SourceInfo<'src> {
    pub fn from_range(input: &'src str, start: usize, end: Option<NonZeroUsize>) -> Self {
        let line_start_info = LinePosInfo::from_offset(input, start);
        let end = end
            .map(NonZeroUsize::into)
            .unwrap_or_else(|| start.saturating_add(1).min(line_start_info.line_range.end));
        let range = start..end;
        let line_end_info = LinePosInfo::from_offset(input, range.end);

        let line_start = Line::new(input, line_start_info.line_range.clone(), &range);
        let line_end = Line::new(input, line_end_info.line_range.clone(), &range);

        Self {
            line_start_info,
            line_end_info,
            line_start,
            line_end,
            full_range: range,
            source: input,
            context_lines: 0,
        }
    }

    fn line_span(&self) -> usize {
        self.line_end_info.line_no - self.line_start_info.line_no
    }

    fn with_context_lines(mut self, ctx_lines: usize) -> Self {
        self.context_lines = (ctx_lines / 2).saturating_sub(self.line_span() / 2);
        self
    }

    fn get_use_highlight<'a, T>(&self, value: &'a T) -> UseHighlight<'a, T> {
        UseHighlight {
            value,
            highlight_range: self.full_range.clone(),
        }
    }
}

fn digits_of(mut n: usize) -> usize {
    let mut count = 1;
    while n / 10 != 0 {
        n /= 10;
        count += 1;
    }
    count
}

impl Display for SourceInfo<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("\n")?;
        let line_no_align =
            digits_of(self.line_start_info.line_no).max(digits_of(self.line_end_info.line_no));
        let col_no_align =
            digits_of(self.line_start_info.col_no).max(digits_of(self.line_end_info.col_no));

        if self.context_lines > 0 {
            f.write_str(HighlightFormatter::RESET)?;
            for (offset, line) in self
                .source
                .lines()
                .skip(
                    self.line_start_info
                        .line_no
                        .saturating_sub(self.context_lines)
                        .saturating_sub(1),
                )
                .take(self.context_lines)
                .enumerate()
            {
                let idx =
                    (offset + self.line_start_info.line_no).saturating_sub(self.context_lines);
                writeln!(f, " {idx:line_no_align$} {:col_no_align$} | {line}", ' ')?;
            }
            f.write_str(HighlightFormatter::HL_COLOR)?;
        }
        match self.line_end_info.line_no - self.line_start_info.line_no {
            // same line
            0 => {
                writeln!(
                    f,
                    " {:3}:{:2} | {}",
                    self.line_start_info.line_no,
                    self.line_start_info.col_no,
                    self.get_use_highlight(&self.line_start)
                )
            }
            more => {
                writeln!(
                    f,
                    " {:3}:{:2} | {}",
                    self.line_start_info.line_no,
                    self.line_start_info.col_no,
                    self.get_use_highlight(&self.line_start)
                )?;
                if more > 1 {
                    for in_between in self
                        .source
                        .lines()
                        .skip(self.line_start_info.line_no - 1)
                        .take(more)
                    {
                        writeln!(f, "   ..   | {}", in_between)?;
                    }
                }

                writeln!(
                    f,
                    " {:3}:{:2} | {}",
                    self.line_end_info.line_no,
                    self.line_end_info.col_no,
                    self.get_use_highlight(&self.line_end)
                )
            }
        }?;

        if self.context_lines > 0 {
            f.write_str(HighlightFormatter::RESET)?;
            for (offset, line) in self
                .source
                .lines()
                .skip(self.line_end_info.line_no)
                .take(self.context_lines)
                .enumerate()
            {
                let idx = offset + self.line_end_info.line_no;
                writeln!(f, " {idx:line_no_align$} {:col_no_align$} |  {line}", ' ')?;
            }
            f.write_str(HighlightFormatter::HL_COLOR)?;
        }
        Ok(())
    }
}

fn main() {
    let Config {
        file,
        begin_offset,
        end_offset,
        context_lines,
        reverse,
    } = Config::from_args();

    let contents = std::fs::read_to_string(file).unwrap();

    if reverse {
        let mut offset = 0;

        let line_no = begin_offset;
        let col_no = end_offset.map(NonZeroUsize::into).unwrap_or(1);

        for _ in 1..line_no {
            offset += contents[offset..].find('\n').map(|x| x + 1).unwrap_or(0);
        }

        offset += col_no;

        println!("{}", offset);
    } else {
        println!(
            "{}",
            SourceInfo::from_range(&contents, begin_offset, end_offset)
                .with_context_lines(context_lines.unwrap_or(0))
        )
    }
}
