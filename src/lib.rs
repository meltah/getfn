//! Utilities for referring to functions by pointer.
//! 
//! Using the `getfn!` macro, you can generate a pair of functions: the first
//! function acts as a "getter" function, and is prefixed with `getfn_`. The
//! second function simply calls the function normally:
//! 
//! ```
//! use getfn::getfn;
//! 
//! getfn! {
//! 	(|| { println!("hi!"); })
//! 	fn my_func();
//! }
//! 
//! my_func(); // hi!
//! let f = getfn_my_func();
//! f(); // hi!
//! ```
//! 
//! You might be wondering why not simply use `let f = my_func;` instead of
//! having a separate getter. The reason is that the resulting function pointer
//! in `f` will not have the exact same address as the one passed into `getfn!`.
//! This is necessary in cases like game modding, where to [hook] a function,
//! you must have its exact address. To aid these usecases, `getfn` also offers
//! a `get_addr!` macro, which is a DSL for getting the address of a function:
//! ```norun
//! // gets the base address of the `MyGame.exe` module using the backend
//! // (currently assumed to be `winapi` accessible from `crate::__getfn_winapi_`)
//! let f = getfn::get_addr!("MyGame.exe" + 0x20bf00);
//! ```
//! These can be combined together using the `symbol_fn!` macro:
//! ```norun
//! use getfn::symbol_fn;
//! use std::ffi::c_void;
//! 
//! symbol_fn! {
//! 	("MyGame.exe" + 0x20bf00)
//! 	extern "C" fn my_game_func(a: i32) -> bool;
//! }
//! println!("{}", my_game_func(5));
//! 
//! let func = getfn_my_game_func();
//! 
//! extern "C" fn detour(a: i32) -> bool { a == 5 }
//! 
//! // .. hook `func` ..
//! let mut orig = std::ptr::null_mut();
//! MH_CreateHook(func as *mut c_void, detour, &mut orig as *mut _ as _);
//! ```
//! [hook]: https://en.wikipedia.org/wiki/Hooking

#![no_std]

pub use paste;

#[macro_export]
macro_rules! getfn {
	() => {};
	(
		($e:expr) $vis:vis $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> ($(extern $cc)? fn($($argtype),*) $(-> $ret)?) { $e }
		}

		$crate::paste::item! {
			$vis fn $name($($arg: $argtype),*) $(-> $ret)? {
				[<getfn_ $name>]()($($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
	(
		($e:expr) $vis:vis unsafe $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> (unsafe $(extern $cc)? fn($($argtype),*) $(-> $ret)?) { $e }
		}

		$crate::paste::item! {
			$vis unsafe fn $name($($arg: $argtype),*) $(-> $ret)? {
				[<getfn_ $name>]()($($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
	(
		($e:expr) $vis:vis $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> ($(extern $cc)? fn($($argtype),*) $(-> $ret)?) { $e }
		}
		$crate::paste::item! {
			$vis fn $name($($arg: $argtype),*) $(-> $ret)? {
				Self::[<getfn_ $name>]()($($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
	(
		($e:expr) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> (unsafe $(extern $cc)? fn($($argtype),*) $(-> $ret)?) { $e }
		}
		$crate::paste::item! {
			$vis unsafe fn $name($($arg: $argtype),*) $(-> $ret)? {
				Self::[<getfn_ $name>]()($($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
	(
		($e:expr) $vis:vis $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> ($crate::_getfn_ret!([$($self)+] $($cc)?, $($ret)?, $($argtype),*)) { $e }
		}
		$crate::paste::item! {
			$vis fn $name($($self)+, $($arg: $argtype),*) $(-> $ret)? {
				Self::[<getfn_ $name>]()($crate::_getfn_get_self_tok!($($self)+), $($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
	(
		($e:expr) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tt:tt)*
	) => {
		$crate::paste::item! {
			#[allow(unused_parens)]
			$vis fn [<getfn_ $name>]() -> (unsafe $crate::_getfn_ret!([$($self)+] $($cc)?, $($ret)?, $($argtype),*)) { $e }
		}
		$crate::paste::item! {
			$vis unsafe fn $name($($self)+, $($arg: $argtype),*) $(-> $ret)? {
				Self::[<getfn_ $name>]()($crate::_getfn_get_self_tok!($($self)+), $($arg),*)
			}
		}

		$crate::getfn!($($tt)*);
	};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _getfn_ret {
	([self] $($cc:literal)?, $($ret:ty)?, $($args:tt)*) => ($(extern $cc)? fn(Self, $($args),*) $(-> $ret)?);
	([& $($lt:lifetime)? self] $($cc:literal)?, $($ret:ty)?, $($args:tt)*) => ($(extern $cc)? fn(&$($lt)? Self, $($args),*) $(-> $ret)?);
	([& $($lt:lifetime)? mut self] $($cc:literal)?, $($ret:ty)?, $($args:tt)*) => ($(extern $cc)? fn(&$($lt)? mut Self, $($args),*) $(-> $ret)?);
}

#[doc(hidden)]
#[macro_export]
macro_rules! _getfn_get_self_tok {
	(& $($lt:lifetime)? mut $t:tt) => ($t);
	(& $($lt:lifetime)? $t:tt) => ($t);
	($t:tt) => ($t);
}

#[macro_export]
macro_rules! alias {
	($name:ident) => ($crate::paste::expr!(crate:: [<__getfn_alias__ $name>] ::value()));
}

#[macro_export]
macro_rules! def_alias {
	() => {};
	($name:ident = $val:expr; $($tt:tt)*) => {
		$crate::paste::item! {
			pub(crate) mod [<__getfn_alias__ $name>] {
				pub fn ___value___() -> *mut () {
					use super::*;
					$val
				}
			}
		}
		$crate::paste::item! {
			use crate:: [<__getfn_alias__ $name>] as _;
		}
	};
	($name:ident: $t:ty = $val:expr; $($tt:tt)*) => {
		$crate::paste::item! {
			pub(crate) mod [<__getfn_alias__ $name>] {
				pub fn ___value___() -> $t {
					use super::*;
					$val
				}
			}
		}
		$crate::paste::item! {
			use crate:: [<__getfn_alias__ $name>] as _;
		}
	}
}

/// DSL for getting addresses
#[macro_export]
macro_rules! get_addr {
	($custom:ident $($tt:tt)*) => {
		$crate::_get_addr0!(
			($crate::alias!($custom))
			$($tt)*
		)
	};

	(($e:expr) $($tt:tt)*) => {
		$crate::_get_addr0!(
			($e)
			$($tt)*
		)
	};

	($module:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			(crate::__getfn_winapi__::um::libloaderapi::GetModuleHandleA(concat!($module, "\0").as_ptr() as _).cast::<()>())
			$($tt)*
		)
	};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _get_addr0 {
	(($v:expr)) => ($v);
	(($v:expr) @ $sym:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			(::core::mem::transmute::<_, *mut ()>(crate::__getfn_winapi__::um::libloaderapi::GetProcAddress($v as *mut _, concat!($sym, "\0").as_ptr() as _)))
			$($tt)*
		)
	};
	(($v:expr) * $($tt:tt)*) => {
		$crate::_get_addr0!(
			(($v as *mut *mut ()).read())
			$($tt)*
		)
	};
	(($v:expr) + $e:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			($v.cast::<u8>().offset($e as isize).cast::<()>())
			$($tt)*
		)
	};
	(($v:expr) + ($e:expr) $($tt:tt)*) => {
		$crate::_get_addr0!(
			($v.cast::<u8>().offset($e as isize).cast::<()>())
			$($tt)*
		)
	};
	(($v:expr) - $e:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			($v.cast::<u8>().offset(-($e as isize)).cast::<()>())
			$($tt)*
		)
	};
	(($v:expr) - ($e:expr) $($tt:tt)*) => {
		$crate::_get_addr0!(
			($v.cast::<u8>().offset(-($e as isize)).cast::<()>())
			$($tt)*
		)
	}
}

#[macro_export]
macro_rules! symbol_fn {
	() => {};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn $name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn $name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn Self::$name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn Self::$name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn Self::$name([$($self)+] $($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),* $(,)?) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::core::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn Self::$name([$($self)+] $($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
}

#[macro_export]
macro_rules! symbol_static {
	() => {};
	(($($tt:tt)*) $vis:vis static $name:ident: $ty:ty; $($tta:tt)*) => {
		$vis const $name: $crate::SymbolStatic<$ty> = unsafe { $crate::SymbolStatic::_new(|| {
			$crate::get_addr!($($tt)*)
		}) };
		$crate::symbol_static!($($tta)*);
	};
	(($($tt:tt)*) $vis:vis static mut $name:ident: $ty:ty; $($tta:tt)*) => {
		::core::compile_error!("static muts are unsupported. consider using atomics or `static X: UnsafeCell<T>`.");
	}
}


// ============= //

use core::marker::PhantomData;
use core::ops::Deref;

#[derive(Copy, Clone)]
pub struct SymbolStatic<T: Sync>(PhantomData<T>, fn() -> *mut ());

impl<T: Sync> Deref for SymbolStatic<T> {
	type Target = T;
	fn deref(&self) -> &T { unsafe { &*(self.1() as *const T) } }
}

impl<T: Sync> SymbolStatic<T> {
	#[doc(hidden)]
	pub const unsafe fn _new(x: fn() -> *mut ()) -> Self {
		Self(PhantomData, x)
	}
}
