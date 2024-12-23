use std::{
    io::{IsTerminal, Write},
    time::Duration,
};
use termcolor::{BufferWriter, Color, ColorChoice, ColorSpec, WriteColor};

pub(crate) fn print_analyzing(text: &str) {
    print_colourful_prefix("Analyzing", Color::Magenta, text)
}

pub(crate) fn print_analyzed(duration: Duration) {
    print_colourful_prefix("Analyzed", Color::Green, &format!("in {}", seconds(duration)))
}

pub(crate) fn print_running(text: &str) {
    print_colourful_prefix("Running", Color::Green, text)
}

#[cfg(feature = "compiler")]
pub(crate) fn print_compiling(text: &str) {
    print_colourful_prefix("Compiling", Color::Cyan, text);
}

#[cfg(feature = "compiler")]
pub(crate) fn print_compiled(duration: Duration) {
    print_colourful_prefix("Compiled", Color::Green, &format!("in {}", seconds(duration)))
}

pub fn seconds(duration: Duration) -> String {
    format!("{:.2}s", duration.as_millis() as f32 / 1000.)
}

pub fn print_colourful_prefix(prefix: &str, color: Color, text: &str) {
    let buffer_writer = stderr_buffer_writer();
    let mut buffer = buffer_writer.buffer();
    buffer
        .set_color(
            ColorSpec::new()
                .set_intense(true)
                .set_bold(true)
                .set_fg(Some(color)),
        )
        .expect("print_colourful_prefix");
    write!(buffer, "{prefix: >11}").expect("print_colourful_prefix");
    buffer
        .set_color(&ColorSpec::new())
        .expect("print_colourful_prefix");
    writeln!(buffer, " {text}").expect("print_colourful_prefix");
    buffer_writer.print(&buffer).expect("print_colourful_prefix");
}

pub fn stderr_buffer_writer() -> BufferWriter {
    BufferWriter::stderr(color_choice())
}

fn colour_forced() -> bool {
    if let Ok(force) = std::env::var("FORCE_COLOR") {
        !force.is_empty()
    } else {
        false
    }
}

fn color_choice() -> ColorChoice {
    if colour_forced() {
        ColorChoice::Always
    } else if std::io::stderr().is_terminal() {
        ColorChoice::Auto
    } else {
        ColorChoice::Never
    }
}