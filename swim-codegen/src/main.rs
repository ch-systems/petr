use std::{fs::File, io::Write};

use cranelift::codegen::ir::{UserExternalNameRef, UserFuncName};
use cranelift::codegen::{CompiledCode, MachBufferFinalized};

fn main() {
    println!("Hello, world!");
}

#[test]
fn new_ir() -> Result<(), Box<dyn std::error::Error>> {
    use cranelift::codegen::{
        ir::{AbiParam, Function, Signature},
        isa,
        settings::{self, Configurable},
    };
    use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
    use cranelift::prelude::*;
    use cranelift_module::{Linkage, Module};
    use cranelift_native::builder as isa_builder;
    use cranelift_object::{ObjectBuilder, ObjectModule};

    // Set up the ISA for the current machine
    let flag_builder = settings::builder();
    let isa_builder = isa_builder()?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    // Set up the object module
    let builder = ObjectBuilder::new(
        isa,
        "my_program.o",
        cranelift_module::default_libcall_names(),
    )?;
    let mut module = ObjectModule::new(builder);

    // Create a function signature
    let mut func_sig = module.make_signature();
    func_sig
        .returns
        .push(AbiParam::new(cranelift::codegen::ir::types::I32));
    let func_id = module.declare_function("_main", Linkage::Export, &func_sig)?;

    // Define the function
    let mut context = module.make_context();
    context.func.signature = func_sig;
    //    context.func.name = cranelift::codegen::ir::ExternalName::user(0, func_id.as_u32());
    context.func.name = UserFuncName::user(0, func_id.as_u32());

    {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_ctx);

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let value = builder.ins().iconst(cranelift::codegen::ir::types::I32, 42);
        builder.ins().return_(&[value]);

        builder.finalize();
    }

    // Define and compile the function
    module.define_function(func_id, &mut context)?;
    module.clear_context(&mut context);
    //    module.finalize_definitions();

    // Write the object file
    let product = module.finish();
    let obj = product.object;

    let mut file = File::create("output.o")?;
    file.write_all(&obj.write().unwrap())?;
    Ok(())
}

/*
#[test]
fn ir() -> Result<(), Box<dyn std::error::Error>> {
    use cranelift::prelude::*;
    use cranelift::{
        codegen::{
            control::ControlPlane,
            entity::EntityRef,
            ir::{types::*, AbiParam, Function, InstBuilder, Signature, UserFuncName},
            isa::CallConv,
            settings,
            verifier::verify_function,
        },
        frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
    };
    /*
    use cranelift_codegen::entity::EntityRef;
    use cranelift_codegen::ir::types::*;
    use cranelift_codegen::ir::{AbiParam, Function, InstBuilder, Signature, UserFuncName};
    use cranelift_codegen::isa::CallConv;
    use cranelift_codegen::settings;
    use cranelift_codegen::verifier::verify_function;
    use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
    */

    let mut sig = Signature::new(CallConv::SystemV);
    sig.returns.push(AbiParam::new(I32));
    sig.params.push(AbiParam::new(I32));
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
    //    let mut func = Function::with_name_signature(UserFuncName::new("test", 0), sig);
    {
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        let block0 = builder.create_block();
        let block1 = builder.create_block();
        let block2 = builder.create_block();
        let block3 = builder.create_block();
        let x = Variable::new(0);
        let y = Variable::new(1);
        let z = Variable::new(2);
        builder.declare_var(x, I32);
        builder.declare_var(y, I32);
        builder.declare_var(z, I32);
        builder.append_block_params_for_function_params(block0);

        builder.switch_to_block(block0);
        builder.seal_block(block0);
        {
            let tmp = builder.block_params(block0)[0]; // the first function parameter
            builder.def_var(x, tmp);
        }
        {
            let tmp = builder.ins().iconst(I32, 2);
            builder.def_var(y, tmp);
        }
        {
            let arg1 = builder.use_var(x);
            let arg2 = builder.use_var(y);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        builder.ins().jump(block1, &[]);

        builder.switch_to_block(block1);
        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(z);
            let tmp = builder.ins().iadd(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().brif(arg, block3, &[], block2, &[]);
        }

        builder.switch_to_block(block2);
        builder.seal_block(block2);
        {
            let arg1 = builder.use_var(z);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(z, tmp);
        }
        {
            let arg = builder.use_var(y);
            builder.ins().return_(&[arg]);
        }

        builder.switch_to_block(block3);
        builder.seal_block(block3);

        {
            let arg1 = builder.use_var(y);
            let arg2 = builder.use_var(x);
            let tmp = builder.ins().isub(arg1, arg2);
            builder.def_var(y, tmp);
        }
        builder.ins().jump(block1, &[]);
        builder.seal_block(block1);

        builder.finalize();
    }

    let flags = settings::Flags::new(settings::builder());
    let res = verify_function(&func, &flags);

    println!("{}", func.display());
    // we have now built and verified IR
    // time to generate native code

    use cranelift::codegen::cursor::Cursor;
    use cranelift::codegen::ir::types::*;
    use cranelift::codegen::isa;
    use cranelift::codegen::settings::Configurable;
    use cranelift_native::builder as native_builder;

    // Set up the ISA for the current machine
    let flag_builder = settings::builder();
    let isa_builder = native_builder()?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;

    // // Set up a function builder to generate the IR
    // let mut builder = FunctionBuilder::new(&mut func, &mut builder_ctx);
    // {
    //     let entry_block = builder.create_block();
    //     builder.switch_to_block(entry_block);
    //     builder.seal_block(entry_block);

    //     // Create an integer constant and return it
    //     let int_val = builder.ins().iconst(I32, 42);
    //     builder.ins().return_(&[int_val]);

    //     // Finish building the function
    //     builder.finalize();
    // }

    // Create a compiler context and compile the function
    let mut comp_ctx = cranelift::codegen::Context::new();
    comp_ctx.func = func;
    // Set up a control plane
    let mut control_plane: ControlPlane = Default::default();
    let mut buf = Default::default();
    let buffer = comp_ctx
        .compile_and_emit(&*isa, &mut buf, &mut control_plane)
        .unwrap();
    //    let buffer = comp_ctx.compile(&*isa, &mut control_plane)?;

    // The output `buffer` contains the machine code
    println!("Generated machine code: {:?}", buffer);

    generate_macos_executable(buffer.clone());

    //    panic!("{}", func.display());
    if let Err(errors) = res {
        panic!("{}", errors);
    }
    Ok(())
}

fn generate_macos_executable(code: CompiledCode) {
    use cranelift::codegen::{
        ir::{AbiParam, Function, Signature},
        isa,
        settings::{self, Configurable},
    };
    use cranelift::frontend::{FunctionBuilder, FunctionBuilderContext};
    use cranelift::prelude::*;
    use cranelift_native::builder as isa_builder;
    use cranelift_object::{ObjectBuilder, ObjectModule};
    // Set up the ISA for the current machine
    let flag_builder = settings::builder();
    let isa_builder = isa_builder().unwrap();
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .unwrap();

    // Set up the object module
    let builder = ObjectBuilder::new(
        isa,
        "my_program.o",
        cranelift_module::default_libcall_names(),
    )
    .unwrap();
    use cranelift_module::Module;
    let mut module = ObjectModule::new(builder);
    // Create a function signature
    let mut func_sig = module.make_signature();
    func_sig
        .returns
        .push(AbiParam::new(cranelift::codegen::ir::types::I32));
    let func_id = module
        .declare_function("_main", cranelift_module::Linkage::Export, &func_sig)
        .unwrap();
    let mut context = module.make_context();
    context.func.signature = func_sig;
    //    context.func.name = todo!();

    {
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_ctx);

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let value = builder.ins().iconst(cranelift::codegen::ir::types::I32, 42);
        builder.ins().return_(&[value]);

        builder.finalize();
    }

    // Define and compile the function
    module.define_function(func_id, &mut context).unwrap();
    module.clear_context(&mut context);
    // Write the object file
    let product = module.finish();
    let obj = product.object;
    let mut buf = vec![];
    obj.emit(&mut buf).unwrap();
    std::fs::write("output.o", buf).unwrap();
}
/*
fn generate_macos_executable(code: CompiledCode) {
    use cranelift_object::{
        write::Object, Architecture, BinaryFormat, Endianness, FileFlags, SectionKind, SymbolFlags,
        SymbolKind, SymbolScope, SymbolSection,
    };
    use std::fs::File;
    use std::io::Write;

    fn create_macho_file(machine_code: &[u8]) -> Result<(), std::io::Error> {
        let mut obj = Object::new(
            BinaryFormat::MachO,
            Architecture::,
            Endianness::Little,
        );

        // Add a __text section containing the machine code
        let section_id = obj.add_section(
            obj.segment_name(cranelift_object::write::StandardSegment::Text)
                .to_vec(),
            b"__text".to_vec(),
            SectionKind::Text,
        );
        // Convert SectionId to SectionIndex
        // let section_index = obj.section_id(section_id); // Convert SectionId to SectionIndex
        // Add a symbol for the entry point
        obj.add_symbol(cranelift_object::write::Symbol {
            name: b"_main".to_vec(),
            value: 0,
            size: machine_code.len() as u64,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: cranelift_object::write::SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });

        let mut file = File::create("output.o")?;
        file.write_all(&obj.write().unwrap())?;

        Ok(())
    }

    // Usage:
    // Assume `buffer.data()` contains your machine code from Cranelift
    let buffer = code.buffer.data();
    create_macho_file(&buffer).unwrap();
}
*/
*/
