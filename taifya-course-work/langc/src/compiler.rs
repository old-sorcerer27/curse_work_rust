use std::{path::PathBuf, process::Command};
use inkwell::{module::Module, targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine}, OptimizationLevel};

pub struct ObjectCompiler;
pub struct ObjectLinker;

impl ObjectCompiler {
    pub fn compile(
        opt_level: OptimizationLevel,
        reloc_mode: RelocMode,
        code_model: CodeModel,
        module: &Module,
        file_type: FileType,
        out: &PathBuf,
    ) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target.create_target_machine(
            &target_triple, 
            "generic", 
            "", 
            opt_level, 
            reloc_mode, 
            code_model
        ).unwrap();

        module.set_data_layout(&target_machine.get_target_data().get_data_layout());
        module.set_triple(&target_triple);
        
        // let buf = target_machine.write_to_memory_buffer(
        //     &module, 
        //     inkwell::targets::FileType::Object
        // ).unwrap();
        // let assembly = target_machine.write_to_memory_buffer(
        //     &module, 
        //     inkwell::targets::FileType::Assembly
        // ).unwrap();

        // let str = str::from_utf8(assembly.as_slice()).unwrap();

        // println!("{}", str);

        target_machine.write_to_file(
            &module, 
            file_type, 
            &out
        ).unwrap();
    }
}

impl ObjectLinker {
    pub fn link(input_file: &PathBuf, opt_level: inkwell::OptimizationLevel, output_file: &PathBuf) -> Result<(), i32> {
        let gcc = Command::new("gcc")
            .arg(input_file)
            .arg(&format!("-O{}", opt_level as isize))
            .arg("-o")
            .arg(output_file)
            .output()
            .expect("Unable to run `gcc` command");

        match gcc.status.success() {
            true => Ok(()),
            false => return Err(gcc.status.code().unwrap())
        }
    }
}