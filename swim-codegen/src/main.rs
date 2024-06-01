fn main() -> Result<(), Box<dyn std::error::Error>> {
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

    // Define the data section with "Hello, world!\n"
    let mut data_ctx = cranelift_module::DataDescription::new();
    let data = "Hello, world!\n\0".as_bytes();
    data_ctx.define(data.to_vec().into_boxed_slice());
    let data_id = module.declare_data("hello_world", Linkage::Local, false, false)?;
    module.define_data(data_id, &data_ctx)?;

    // Create a function signature
    let mut func_sig = module.make_signature();
    func_sig
        .returns
        .push(AbiParam::new(cranelift::codegen::ir::types::I32));
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
        let puts_func_ref = module.declare_func_in_func(puts_func, &mut builder.func);

        // Reference the data section
        let data_ref = module.declare_data_in_func(data_id, &mut builder.func);
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
        .arg("-v")
        .status()?;

    use std::fs;
    use std::os::unix::fs::PermissionsExt;

    // Set the output file to be executable
    let mut perms = fs::metadata("output")?.permissions();
    perms.set_mode(0o755);
    fs::set_permissions("output", perms)?;
    // Make the output file executable

    Ok(())
}
