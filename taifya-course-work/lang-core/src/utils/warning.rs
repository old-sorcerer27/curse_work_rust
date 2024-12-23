use std::{path::PathBuf, rc::Rc, sync::{atomic::{AtomicUsize, Ordering}, Arc, RwLock}};

use termcolor::Buffer;

use crate::analyzer::error::Warning as AnalyzerWarning;
use super::diagnostic::{Diagnostic, Label, Level, Location};

pub trait WarningEmitterIO {
    fn emit_warning(&self, warning: Warning);
}

#[derive(Debug, Clone, Copy)]
pub struct NullWarningEmitterIO;

impl WarningEmitterIO for NullWarningEmitterIO {
    fn emit_warning(&self, _warning: Warning) {}
}

#[derive(Debug, Default, Clone)]
pub struct VectorWarningEmitterIO {
    pub warnings: Arc<RwLock<Vec<Warning>>>
}

impl VectorWarningEmitterIO {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn take(&self) -> Vec<Warning> {
        let mut warnings = self.write_lock();
        std::mem::take(&mut *warnings)
    }

    pub fn reset(&self) {
        let mut warnings = self.write_lock();
        warnings.clear();
    }

    pub fn pop(&self) -> Option<Warning> {
        let mut warnings = self.write_lock();
        warnings.pop()
    }

    fn write_lock(&self) -> std::sync::RwLockWriteGuard<'_, Vec<Warning>> {
        self.warnings.write().expect("Vector lock poisoned")
    }
}

impl WarningEmitterIO for VectorWarningEmitterIO {
    fn emit_warning(&self, warning: Warning) {
        let mut warnings = self.write_lock();

        warnings.push(warning);
    }
}

pub struct WarningEmitter {
    count: Arc<AtomicUsize>,
    emitter: Rc<dyn WarningEmitterIO>
}

impl WarningEmitter {
    pub fn new(emitter: Rc<dyn WarningEmitterIO>) -> Self {
        Self {
            count: Arc::new(AtomicUsize::new(0)),
            emitter,
        }
    }

    pub fn null() -> Self {
        Self::new(Rc::new(NullWarningEmitterIO))
    }

    pub fn reset_count(&self) {
        self.count.store(0, Ordering::Relaxed);
    }

    pub fn count(&self) -> usize {
        self.count.load(Ordering::Relaxed)
    }

    pub fn emit(&self, warning: Warning) {
        _ = self.count.fetch_add(1, Ordering::Relaxed);
        self.emitter.emit_warning(warning);
    }
}

pub struct TypeWarningEmitter {
    module_path: PathBuf,
    module_src: String,
    emitter: WarningEmitter,
}

impl TypeWarningEmitter {
    pub fn new(
        module_path: PathBuf, 
        module_src: String, 
        emitter: WarningEmitter
    ) -> Self {
        Self {
            module_path,
            module_src,
            emitter,
        }
    }

    pub fn null() -> Self {
        Self {
            module_path: PathBuf::new(),
            module_src: String::from(""),
            emitter: WarningEmitter::new(Rc::new(NullWarningEmitterIO)),
        }
    }

    pub fn emit(&self, warning: AnalyzerWarning) {
        self.emitter.emit(Warning::Type {
            path: self.module_path.clone(),
            src: self.module_src.clone(),
            warning,
        });
    }
}

#[derive(Debug, Clone)]
pub enum Warning {
    Type {
        path: PathBuf,
        src: String,
        warning: AnalyzerWarning
    }
}

impl Warning {
    pub fn pretty_string(&self) -> String {
        let mut nocolor = Buffer::no_color();
        self.pretty(&mut nocolor);
        String::from_utf8(nocolor.into_inner()).expect("Error printing produced invalid utf8")
    }

    pub fn pretty(&self, buf: &mut Buffer) {
        use std::io::Write;

        self.to_diagnostic().write(buf);
        buf.write_all(b"\n")
            .expect("error pretty buffer write space after");
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Warning::Type {
                path,
                src,
                warning
            } => match warning {
                AnalyzerWarning::UnreachableIfClause { location } => {
                    Diagnostic {
                        title: "Unreachable if clause".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                },
                AnalyzerWarning::EmptyDeclaration { location } => {
                    Diagnostic {
                        title: "Empty declaration".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                }
                AnalyzerWarning::UnreachableElseClause { location } => {
                    Diagnostic {
                        title: "Unreachable else clause".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                },
                AnalyzerWarning::UnusedVariable { location } => {
                    Diagnostic {
                        title: "Unused variable".into(),
                        text: "".into(),
                        // hint: Some("You can safely remove it.".into()),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: Some("This value is never used".into()),
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                },
                AnalyzerWarning::InfiniteLoop { location } => {
                    Diagnostic {
                        title: "Infinite loop".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                },
                AnalyzerWarning::UnreachableWhileClause { location } => {
                    Diagnostic {
                        title: "Unreachable while clause".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                },
                AnalyzerWarning::UnreachableCode { location } => {
                    Diagnostic {
                        title: "Unreachable code".into(),
                        text: "".into(),
                        level: Level::Warning,
                        location: Some(Location {
                            src: &src,
                            path: path.to_path_buf(),
                            label: Label {
                                text: None,
                                span: *location,
                            },
                            extra_labels: vec![]
                        }),
                    }
                }
            }
        }
    }
}