use ahash::HashMap;
use stringleton::{Symbol, sym};

use crate::Value;

pub fn default_global_constants() -> &'static HashMap<Symbol, Value> {
    static GLOBAL_CONSTANTS: std::sync::OnceLock<HashMap<Symbol, Value>> =
        std::sync::OnceLock::new();
    GLOBAL_CONSTANTS.get_or_init(|| {
        let mut map = HashMap::default();
        map.extend([
            (sym!(EMPTY), Value::from(String::new())),
            (sym!(EXE_SUFFIX), Value::from(exe_suffix().to_owned())),
            (sym!(DYLIB_PREFIX), Value::from(dylib_prefix().to_owned())),
            (sym!(DYLIB_SUFFIX), Value::from(dylib_suffix().to_owned())),
            (
                sym!(STATICLIB_PREFIX),
                Value::from(staticlib_prefix().to_owned()),
            ),
            (
                sym!(STATICLIB_SUFFIX),
                Value::from(staticlib_suffix().to_owned()),
            ),
            (sym!(OS), Value::from(current_os().to_owned())),
            (sym!(OS_FAMILY), Value::from(current_os_family().to_owned())),
            (sym!(ARCH), Value::from(current_arch().to_owned())),
            (
                sym!(ARCH_FAMILY),
                Value::from(current_arch_family().to_owned()),
            ),
        ]);
        map
    })
}

#[must_use]
pub const fn current_os() -> &'static str {
    if cfg!(target_os = "windows") {
        "windows"
    } else if cfg!(target_os = "macos") {
        "macos"
    } else if cfg!(target_os = "ios") {
        "ios"
    } else if cfg!(target_os = "linux") {
        "linux"
    } else if cfg!(target_os = "android") {
        "android"
    } else if cfg!(target_os = "freebsd") {
        "freebsd"
    } else if cfg!(target_os = "dragonfly") {
        "dragonfly"
    } else if cfg!(target_os = "openbsd") {
        "openbsd"
    } else if cfg!(target_os = "netbsd") {
        "netbsd"
    } else if cfg!(target_family = "wasm") {
        "wasm-wasi"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_os_family() -> &'static str {
    if cfg!(target_family = "unix") {
        "unix"
    } else if cfg!(target_family = "windows") {
        "windows"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_arch() -> &'static str {
    if cfg!(target_arch = "x86") {
        "x86"
    } else if cfg!(target_arch = "x86_64") {
        "x86_64"
    } else if cfg!(target_arch = "mips") {
        "mips"
    } else if cfg!(target_arch = "powerpc") {
        "powerpc"
    } else if cfg!(target_arch = "powerpc64") {
        "powerpc64"
    } else if cfg!(target_arch = "arm") {
        "arm"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn current_arch_family() -> &'static str {
    if cfg!(any(target_arch = "x86", target_arch = "x86_64")) {
        "x86"
    } else if cfg!(target_arch = "mips") {
        "mips"
    } else if cfg!(any(target_arch = "powerpc", target_arch = "powerpc64")) {
        "powerpc"
    } else if cfg!(any(target_arch = "arm", target_arch = "aarch64")) {
        "arm"
    } else if cfg!(target_family = "wasm") {
        "wasm"
    } else {
        "none"
    }
}

#[must_use]
pub const fn exe_suffix() -> &'static str {
    if cfg!(windows) { ".exe" } else { "" }
}

#[must_use]
pub const fn dylib_prefix() -> &'static str {
    if cfg!(windows) { "" } else { "lib" }
}

#[must_use]
pub const fn dylib_suffix() -> &'static str {
    if cfg!(windows) {
        ".dll"
    } else if cfg!(any(target_os = "macos", target_os = "ios")) {
        ".dylib"
    } else {
        ".so"
    }
}

#[must_use]
pub const fn staticlib_prefix() -> &'static str {
    if cfg!(windows) { "" } else { "lib" }
}

#[must_use]
pub const fn staticlib_suffix() -> &'static str {
    if cfg!(windows) { ".lib" } else { ".a" }
}
