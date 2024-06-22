use std::process::Command;

use cranelift::{
    codegen::settings::{self, Configurable},
    frontend::{FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{DataId, Linkage, Module};
use cranelift_native::builder as isa_builder;
use cranelift_object::{object::write::Object, ObjectBuilder, ObjectModule};
use error::{IrError, IrErrorKind};

mod error {
    use miette::Diagnostic;
    use thiserror::Error;

    #[derive(Debug, Error, Diagnostic)]
    pub struct IrError {
        kind: IrErrorKind,
    }

    impl std::fmt::Display for IrError {
        fn fmt(
            &self,
            f: &mut std::fmt::Formatter<'_>,
        ) -> std::fmt::Result {
            write!(f, "{:?}", self.kind)
        }
    }

    #[derive(Debug, Error, Diagnostic)]
    pub enum IrErrorKind {
        #[error("petr-IR error: {0}")]
        GenericIRError(String),
        #[error(transparent)]
        ModuleError(#[from] cranelift_module::ModuleError),
    }

    impl<T: Into<IrErrorKind>> From<T> for IrError {
        fn from(value: T) -> Self {
            Self { kind: value.into() }
        }
    }
}
pub struct IrContext {
    module: ObjectModule,
}

impl IrContext {
    // TODO: this is probably aarch64/mac specific.
    // Need to make this platform independent. but it might already be.
    pub fn new(file_name: &str) -> Result<Self, IrError> {
        // Set up the ISA for the current machine
        let mut flag_builder = settings::builder();
        flag_builder.enable("is_pic").unwrap();

        let isa_builder = isa_builder().map_err(ToString::to_string).map_err(IrErrorKind::GenericIRError)?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();

        let builder = ObjectBuilder::new(isa, file_name, cranelift_module::default_libcall_names()).expect("TODO");

        let module = ObjectModule::new(builder);
        Ok(Self { module })
    }

    pub fn insert_data(
        &mut self,
        data: Box<[u8]>,
        data_name: &str,
        is_writable: bool,
    ) -> Result<DataId, IrError> {
        // Define the data section with "Hello, world!\n"
        let mut data_ctx = cranelift_module::DataDescription::new();
        data_ctx.define(data);
        let data_id = self.module.declare_data(data_name, Linkage::Local, is_writable, false)?;
        self.module.define_data(data_id, &data_ctx).expect("TODO");
        Ok(data_id)
    }

    // TODO: lowered function type?
    pub fn add_function(
        &mut self,
        func_name: &str,
        function: petr_ir::Function,
    ) -> Result<(), IrError> {
        let sig = self.module.make_signature();
        let func_id = self
            .module
            .declare_function(func_name, Linkage::Local, &sig)
            .map_err(|e| IrErrorKind::GenericIRError(e.to_string()))?;
        // func_sig
        //     .returns
        //     .push(AbiParam::new(cranelift::codegen::ir::types::I32));
        let mut ctx = self.module.make_context();
        // TODO: diff between ctx.func.signature and builder.func.signature?
        ctx.func.signature = sig.clone();
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        builder.func.signature = sig;

        self.lower_function_body(&function, &mut builder);
        self.module
            .define_function(func_id, &mut ctx)
            .map_err(|e| IrErrorKind::GenericIRError(e.to_string()))?;
        self.module.clear_context(&mut ctx);

        // module.define_function(func_id, &mut ctx

        Ok(())
    }

    pub fn into_object_file(self) -> Result<Object<'static>, IrError> {
        let product = self.module.finish();
        let obj = product.object;
        Ok(obj)
    }

    pub fn add_main_function(&mut self) -> Result<(), IrError> {
        // let func_id = module.declare_function("main", Linkage::Export, &func_sig)?;
        todo!()
    }

    fn lower_function_body(
        &self,
        function: &petr_ir::Function,
        builder: &mut FunctionBuilder,
    ) -> Result<(), IrError> {
        todo!()
        // let entry_block = builder.create_block();
        // builder.switch_to_block(entry_block);
        // builder.seal_block(entry_block);
        // let mut puts_sig = module.make_signature();
        // puts_sig.params.push(AbiParam::new(types::I64)); // pointer to char as parameter
        // puts_sig.returns.push(AbiParam::new(types::I32)); // return type of `puts`
        // let puts_func = module.declare_function("puts", Linkage::Import, &puts_sig)?;
        // // get func ref for func id
        // let puts_func_ref = module.declare_func_in_func(puts_func, builder.func);

        // // Reference the data section
        // let data_ref = module.declare_data_in_func(data_id, builder.func);
        // let base_addr = builder.ins().global_value(types::I64, data_ref);

        // // Call `puts` with the string address
        // let call = builder.ins().call(puts_func_ref, &[base_addr]);

        // // Return from the function
        // let ret_val = builder.inst_results(call)[0];
        // builder.ins().return_(&[ret_val]);

        // builder.finalize();
    }
}

fn write_obj_file(
    file_name: &str,
    obj: Object,
) -> Result<(), IrError> {
    let mut file = std::fs::File::create(file_name).expect("TODO errs");
    use std::io::Write;

    file.write_all(&obj.write().unwrap()).expect("TODO errs");
    Ok(())
}

fn link_for_mac(
    obj_file_name: &str,
    output_file_name: &str,
) -> Result<(), IrError> {
    // Link the object file using clang
    Command::new("clang")
        .arg("output.o")
        .arg("-o")
        .arg("output")
        .arg("-Wl")
        .arg("-ld_classic")
        .arg("-v")
        .status()
        .expect("TODO errs");
    use std::{fs, os::unix::fs::PermissionsExt};

    // Set the output file to be executable
    let mut perms = fs::metadata("output").expect("TODO errs").permissions();
    perms.set_mode(0o755);
    fs::set_permissions("output", perms).expect("TODO errs");
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    use cranelift::prelude::*;
    use cranelift_module::{Linkage, Module};
    use cranelift_native::builder as isa_builder;
    use cranelift_object::{ObjectBuilder, ObjectModule};

    // Set up the ISA for the current machine
    let mut flag_builder = settings::builder();
    flag_builder.enable("is_pic").unwrap();

    let isa_builder = isa_builder()?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();

    // Set up the object module
    let builder = ObjectBuilder::new(isa, "my_program.o", cranelift_module::default_libcall_names())?;
    let mut module = ObjectModule::new(builder);

    // Define the data section with "Hello, world!\n"
    let mut data_ctx = cranelift_module::DataDescription::new();
    let data = "Hello, world!\n\0".as_bytes();
    data_ctx.define(data.to_vec().into_boxed_slice());
    let data_id = module.declare_data("hello_world", Linkage::Local, false, false)?;
    module.define_data(data_id, &data_ctx)?;

    // Create a function signature
    let mut func_sig = module.make_signature();
    func_sig.returns.push(AbiParam::new(cranelift::codegen::ir::types::I32));
    let func_id = module.declare_function("main", Linkage::Export, &func_sig)?;

    // Define the function
    let mut context = module.make_context();
    context.func.signature = func_sig;

    {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_ctx);
        //        todo!("Alignment issues?");

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        let mut puts_sig = module.make_signature();
        puts_sig.params.push(AbiParam::new(types::I64)); // pointer to char as parameter
        puts_sig.returns.push(AbiParam::new(types::I32)); // return type of `puts`
        let puts_func = module.declare_function("puts", Linkage::Import, &puts_sig)?;
        // get func ref for func id
        let puts_func_ref = module.declare_func_in_func(puts_func, builder.func);

        // Reference the data section
        let data_ref = module.declare_data_in_func(data_id, builder.func);
        let base_addr = builder.ins().global_value(types::I64, data_ref);

        // Call `puts` with the string address
        let call = builder.ins().call(puts_func_ref, &[base_addr]);

        // Return from the function
        let ret_val = builder.inst_results(call)[0];
        builder.ins().return_(&[ret_val]);

        builder.finalize();
    }

    // Define and compile the function
    module.define_function(func_id, &mut context)?;
    module.clear_context(&mut context);

    // Write the object file
    let product = module.finish();
    let obj = product.object;

    let mut file = std::fs::File::create("output.o")?;
    use std::io::Write;

    file.write_all(&obj.write().unwrap())?;

    // make the file executable
    use std::process::Command;

    // Link the object file using clang
    Command::new("clang")
        .arg("output.o")
        .arg("-o")
        .arg("output")
        .arg("-Wl")
        .arg("-ld_classic")
        .arg("-v")
        .status()?;

    use std::{fs, os::unix::fs::PermissionsExt};

    // Set the output file to be executable
    let mut perms = fs::metadata("output")?.permissions();
    perms.set_mode(0o755);
    fs::set_permissions("output", perms)?;
    // Make the output file executable

    Ok(())
}
