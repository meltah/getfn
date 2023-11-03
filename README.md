[![Crates.io](https://img.shields.io/crates/v/getfn.svg)](https://crates.io/crates/getfn)

# getfn

Utilities for referring to functions by pointer.

Using the `getfn!` macro, you can generate a pair of functions: the first
function acts as a "getter" function, and is prefixed with `getfn_`. The
second function simply calls the function normally:

```rust
use getfn::getfn;

getfn! {
	(|| { println!("hi!"); })
	fn my_func();
}

my_func(); // hi!
let f = getfn_my_func();
f(); // hi!
```

You might be wondering why not simply use `let f = my_func;` instead of
having a separate getter. The reason is that the resulting function pointer
in `f` will not have the exact same address as the one passed into `getfn!`.
This is necessary in cases like game modding, where to [hook] a function,
you must have its exact address. To aid these usecases, `getfn` also offers
a `get_addr!` macro, which is a DSL for getting the address of a function:
```norun
// gets the base address of the `MyGame.exe` module using the backend
// specified via features (currently supported are `winapi` and `libc`).
let f = getfn::get_addr!("MyGame.exe" + 0x20bf00);
```
These can be combined together using the `symbol_fn!` macro:
```norun
use getfn::symbol_fn;
use std::ffi::c_void;

symbol_fn! {
	("MyGame.exe" + 0x20bf00)
	extern "C" fn my_game_func(a: i32) -> bool;
}
println!("{}", my_game_func(5));

let func = getfn_my_game_func();

extern "C" fn detour(a: i32) -> bool { a == 5 }

// .. hook `func` ..
let mut orig = std::ptr::null_mut();
MH_CreateHook(func as *mut c_void, detour, &mut orig as *mut _ as _);
```
[hook]: https://en.wikipedia.org/wiki/Hooking

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
* MIT license ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.
