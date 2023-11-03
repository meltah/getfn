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
			(crate::__getfn_winapi__::um::libloaderapi::GetModuleHandleA(concat!($module, "\0").as_ptr() as _))
			$($tt)*
		)
	};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _get_addr0 {
	(($v:expr)) => ({
		#[allow(unused_unsafe)] unsafe { ::core::mem::transmute::<_, *mut ()>($v) }
	});
	(($v:expr) @ $sym:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			(crate::__getfn_winapi__::um::libloaderapi::GetProcAddress(::core::mem::transmute($v), concat!($sym, "\0").as_ptr() as _))
			$($tt)*
		)
	};
	(($v:expr) * $($tt:tt)*) => {
		$crate::_get_addr0!(
			(*::core::mem::transmute::<_, *mut usize>($v))
			$($tt)*
		)
	};
	(($v:expr) + $e:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			(::core::mem::transmute::<_, usize>($v) + ($e as usize))
			$($tt)*
		)
	};
	(($v:expr) + ($e:expr) $($tt:tt)*) => {
		$crate::_get_addr0!(
			(::core::mem::transmute::<_, usize>($v) + ($e as usize))
			$($tt)*
		)
	};
	(($v:expr) - $e:literal $($tt:tt)*) => {
		$crate::_get_addr0!(
			(::core::mem::transmute::<_, usize>($v) - ($e as usize))
			$($tt)*
		)
	};
	(($v:expr) - ($e:expr) $($tt:tt)*) => {
		$crate::_get_addr0!(
			(::core::mem::transmute::<_, usize>($v) - ($e as usize))
			$($tt)*
		)
	}
}

#[macro_export]
macro_rules! symbol_fn {
	() => {};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn $name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn $name:ident($($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn $name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn Self::$name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident($($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn Self::$name($($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis $(extern $cc)? fn Self::$name([$($self)+] $($arg : $argtype),*) $(-> $ret)?;
		}

		$crate::symbol_fn!($($tta)*);
	};
	(
		($($tt:tt)*) $vis:vis unsafe $(extern $cc:literal)? fn Self::$name:ident([$($self:tt)+] $($arg:ident : $argtype:ty),*) $(-> $ret:ty)?;
		$($tta:tt)*
	) => {
		$crate::getfn! {
			(#[allow(unused_unsafe)] unsafe { ::std::mem::transmute($crate::get_addr!($($tt)*)) }) $vis unsafe $(extern $cc)? fn Self::$name([$($self)+] $($arg : $argtype),*) $(-> $ret)?;
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
