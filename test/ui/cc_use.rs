cc_use!("cc_use.h", A, Zed, B);
cc_use!("cc_use.h" in libfoo, B);
cc_use!("cc_use.h" in "libfoo", B, Foo::C, Foo::D, Bar::E);
cc_use!(<cc_use.h>, A);
cc_use!("cc_use.h", in "libfoo", B);
cc_use!("cc_use.h"; B);
cc_use!("cc_use.h" on "libfoo", B);
cc_use!("nonexistent.h" in "libfoo", B); // TODO
