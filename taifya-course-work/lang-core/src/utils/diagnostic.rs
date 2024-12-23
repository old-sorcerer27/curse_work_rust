use std::path::PathBuf;
pub use codespan_reporting::diagnostic::{LabelStyle, Severity};
use codespan_reporting::{diagnostic::Label as CodespanLabel, files::SimpleFiles};
use termcolor::Buffer;
use super::src_span::SrcSpan;

pub enum Level {
    Warning,
    Error,
}

pub struct Label {
    pub text: Option<String>,
    pub span: SrcSpan
}

impl Label {
    pub fn to_codespan_label(&self, file_id: usize, label_style: Option<LabelStyle>) -> CodespanLabel<usize> {
        let label = CodespanLabel::new(
            label_style.unwrap_or(LabelStyle::Primary),
            file_id,
            (self.span.start as usize)..(self.span.end as usize),
        );

        match &self.text {
            None => label,
            Some(text) => label.with_message(text.clone()),
        }
    }
}

pub struct Location<'a> {
    pub src: &'a str,   
    pub path: PathBuf,
    pub label: Label,
    pub extra_labels: Vec<Label>,
}

pub struct Diagnostic<'a> {
    pub title: String,
    pub text: String,
    pub level: Level,
    pub location: Option<Location<'a>>
}

impl<'a> Diagnostic<'a> {
    pub fn write(&self, buf: &mut Buffer) {
        use std::io::Write;

        match &self.location {
            Some(location) => self.write_span(location, buf),
            None => self.write_title(buf),
        }

        if self.text.len() > 0 {
            writeln!(buf, "{}", self.text).expect("text write")
        }
    }

    pub fn write_span(&self, location: &Location, buf: &mut Buffer) {
        let mut files = SimpleFiles::new();

        let location_path = location.path.to_str().unwrap();
        let location_src = location.src;

        let file_id = files.add(location_path, location_src);

        let mut labels = vec![location.label.to_codespan_label(file_id, None)];

        location.extra_labels.iter()
            .for_each(|label| {
                labels.push(label.to_codespan_label(file_id, Some(LabelStyle::Secondary)))
            });

        let severity = match self.level {
            Level::Error => Severity::Error,
            Level::Warning => Severity::Warning,
        };

        let diagnostic = codespan_reporting::diagnostic::Diagnostic::new(severity)
            .with_message(&self.title)
            .with_labels(labels);

        let config = codespan_reporting::term::Config::default();
        codespan_reporting::term::emit(buf, &config, &files, &diagnostic)
            .expect("write_diagnostic");
    }

    pub fn write_title(&self, buf: &mut Buffer) {
        use std::io::Write;
        use termcolor::{Color, ColorSpec, WriteColor};

        let (kind, colour) = match self.level {
            Level::Error => ("error", Color::Red),
            Level::Warning => ("warning", Color::Yellow),
        };

        buf.set_color(ColorSpec::new().set_bold(true).set_fg(Some(colour)))
            .expect("write_title_color1");
        write!(buf, "{kind}").expect("write_title_kind");

        buf.set_color(ColorSpec::new().set_bold(true))
            .expect("write_title_color2");
        write!(buf, ": {}\n\n", self.title).expect("write_title_title");

        buf.set_color(&ColorSpec::new())
            .expect("write_title_reset");
    }
}