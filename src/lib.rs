// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
#![deny(clippy::unnecessary_wraps)]
#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]

//! Trait and macros to represent Rust errors in JavaScript.
//!
//! ## The [`JsError`] macro
//!
//! Macro to define the `JsErrorClass` trait on a struct or enum.
//!
//! The macro does not provide functionality to related to the `get_message`
//! function, as one can combine the [`thiserror`](https://crates.io/crates/thiserror) well with this macro.
//!
//! ### Attributes
//!
//! #### `#[class]`
//! This attribute accepts 3 possible kinds of value:
//!   1. `GENERIC`, `TYPE`, and a few more that are defined in the `builtin_classes`
//!      module, without the `_ERROR` suffix.
//!   2. A text value ie `"NotFound"`. If a text value is passed that is a valid
//!      builtin (see the previous point), it will error out as the special
//!      identifiers are preferred to avoid mistakes.
//!   3. `inherit`: this will inherit the class from whatever field is marked with
//!      the `#[inherit]` attribute. Alternatively, the `#[inherit]` attribute
//!      can be omitted if only one field is present in the enum variant or struct.
//!      This value is inferred if the class attribute is missing and only a single
//!      field is present on a struct, however for enums this inferring is not done.
//!
//! #### `#[property]`
//! This attribute allows defining fields as additional properties that should be
//! defined on the JavaScript error.
//!
//! The type of the field needs to implement a `.to_string()` function for it
//! being able to be inherited.
//!
//! #### `#[inherit]`
//! This attribute allows defining a field that should be used to inherit the class
//! and properties.
//!
//! This is inferred if only one field is present in the enum variant or struct.
//!
//! The class is only inherited if the `class` attribute is set to `inherit`.
//!
//! ### Examples
//!
//! #### Basic usage
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! pub enum SomeError {
//!   #[class(generic)]
//!   #[error("Failure")]
//!   Failure,
//!   #[class(inherit)]
//!   #[error(transparent)]
//!   Io(#[inherit] std::io::Error),
//! }
//! ```
//!
//! #### Top-level class
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[class(generic)]
//! pub enum SomeError {
//!   #[error("Failure")]
//!   Failure,
//!   #[class(inherit)] // overwrite the top-level
//!   #[error(transparent)]
//!   Io(#[inherit] std::io::Error),
//! }
//! ```
//!
//! #### Defining properties
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[class(generic)]
//! pub enum SomeError {
//!   #[error("Failure")]
//!   Failure {
//!     #[property]
//!     code: u32,
//!   },
//!   #[error("Warning")]
//!   Warning(#[property = "code"] u32),
//!   #[class(inherit)] // inherit properties from `std::io::Error`
//!   #[error(transparent)]
//!   Io(#[inherit] std::io::Error),
//! }
//! ```
//!
//! #### Inferred inheritance
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[error("My io error")]
//! pub struct SomeError(std::io::Error);
//! ```
//!
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[class(inherit)]
//! #[error("My io error")]
//! pub struct SomeError(std::io::Error);
//! ```
//!
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[class(generic)] // don't inherit the error
//! #[error("My io error")]
//! pub struct SomeError(std::io::Error);
//! ```
//!
//! ```
//! #[derive(Debug, thiserror::Error, deno_error::JsError)]
//! #[class(type)]
//! pub enum SomeError {
//!   #[error("Failure")]
//!   Failure,
//!   #[class(inherit)]
//!   #[error(transparent)]
//!   Io(std::io::Error),
//! }
//! ```

mod error_codes;

pub use deno_error_macro::*;
pub use error_codes::*;
use std::borrow::Cow;

/// Various built-in error classes, mainly related to the JavaScript specification.
/// May include some error classes that are non-standard.
pub mod builtin_classes {
  // keep in sync with macros/lib.rs
  pub const GENERIC_ERROR: &str = "Error";
  pub const RANGE_ERROR: &str = "RangeError";
  pub const TYPE_ERROR: &str = "TypeError";
  pub const SYNTAX_ERROR: &str = "SyntaxError";
  pub const URI_ERROR: &str = "URIError";
  pub const REFERENCE_ERROR: &str = "ReferenceError";

  /// Non-standard
  pub const NOT_SUPPORTED_ERROR: &str = "NotSupported";
}
use builtin_classes::*;

/// Trait to implement how an error should be represented in JavaScript.
///
/// **Note**:
/// it is not recommended to manually implement this type, but instead
/// rather use the [`JsError`] macro.
pub trait JsErrorClass: std::error::Error + Send + Sync + 'static {
  /// Represents the error class used in JavaScript side.
  fn get_class(&self) -> &'static str;

  /// Represents the error message used in JavaScript side.
  fn get_message(&self) -> Cow<'static, str>;

  /// Additional properties that should be defined on the error in JavaScript side.
  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>>;
}

/// Macro which lets you wrap an existing error in a new error that implements
/// the [`JsErrorClass`] trait. This macro however does currently not support
/// the special identifiers that the [`JsError`] macro supports.
///
/// ## Examples
///
/// ```rust
/// # use deno_error::js_error_wrapper;
/// js_error_wrapper!(std::net::AddrParseError, JsAddrParseError, "TypeError");
/// ```
///
/// ```rust
/// # use deno_error::js_error_wrapper;
/// js_error_wrapper!(std::net::AddrParseError, JsAddrParseError, |err| {
///   // match or do some logic to get the error class
///   "TypeError"
/// });
/// ```
#[macro_export]
macro_rules! js_error_wrapper {
  ($err_path:path, $err_name:ident, $js_err_type:tt) => {
    deno_error::js_error_wrapper!($err_path, $err_name, |_error| $js_err_type);
  };
  ($err_path:path, $err_name:ident, |$inner:ident| $js_err_type:tt) => {
    #[derive(Debug)]
    pub struct $err_name(pub $err_path);
    impl From<$err_path> for $err_name {
      fn from(err: $err_path) -> Self {
        Self(err)
      }
    }
    impl $err_name {
      pub fn get_error_class($inner: &$err_path) -> &'static str {
        $js_err_type
      }
    }
    impl std::error::Error for $err_name {
      fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        std::error::Error::source(&self.0)
      }
    }
    impl std::fmt::Display for $err_name {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0, f)
      }
    }
    impl deno_error::JsErrorClass for $err_name {
      fn get_class(&self) -> &'static str {
        Self::get_error_class(&self.0)
      }
      fn get_message(&self) -> std::borrow::Cow<'static, str> {
        self.to_string().into()
      }
      fn get_additional_properties(
        &self,
      ) -> Option<
        Vec<(
          std::borrow::Cow<'static, str>,
          std::borrow::Cow<'static, str>,
        )>,
      > {
        None
      }
    }
    impl std::ops::Deref for $err_name {
      type Target = $err_path;

      fn deref(&self) -> &Self::Target {
        &self.0
      }
    }
  };
}

impl JsErrorClass for std::io::Error {
  fn get_class(&self) -> &'static str {
    use std::io::ErrorKind::*;

    match self.kind() {
      NotFound => "NotFound",
      PermissionDenied => "PermissionDenied",
      ConnectionRefused => "ConnectionRefused",
      ConnectionReset => "ConnectionReset",
      ConnectionAborted => "ConnectionAborted",
      NotConnected => "NotConnected",
      AddrInUse => "AddrInUse",
      AddrNotAvailable => "AddrNotAvailable",
      BrokenPipe => "BrokenPipe",
      AlreadyExists => "AlreadyExists",
      InvalidInput => TYPE_ERROR,
      InvalidData => "InvalidData",
      TimedOut => "TimedOut",
      Interrupted => "Interrupted",
      WriteZero => "WriteZero",
      UnexpectedEof => "UnexpectedEof",
      Other => GENERIC_ERROR,
      WouldBlock => "WouldBlock",
      kind => {
        let kind_str = kind.to_string();
        match kind_str.as_str() {
          "FilesystemLoop" => "FilesystemLoop",
          "IsADirectory" => "IsADirectory",
          "NetworkUnreachable" => "NetworkUnreachable",
          "NotADirectory" => "NotADirectory",
          _ => GENERIC_ERROR,
        }
      }
    }
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    get_error_code(self).map(|code| vec![("code".into(), code.into())])
  }
}

impl JsErrorClass for std::env::VarError {
  fn get_class(&self) -> &'static str {
    match self {
      std::env::VarError::NotPresent => "NotFound",
      std::env::VarError::NotUnicode(..) => "InvalidData",
    }
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

impl JsErrorClass for std::sync::mpsc::RecvError {
  fn get_class(&self) -> &'static str {
    GENERIC_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

impl JsErrorClass for std::str::Utf8Error {
  fn get_class(&self) -> &'static str {
    GENERIC_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

impl JsErrorClass for std::num::TryFromIntError {
  fn get_class(&self) -> &'static str {
    TYPE_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

#[cfg(all(feature = "serde", feature = "serde_json"))]
impl JsErrorClass for serde_json::Error {
  fn get_class(&self) -> &'static str {
    use serde::de::StdError;
    use serde_json::error::*;

    match self.classify() {
      Category::Io => self
        .source()
        .and_then(|e| e.downcast_ref::<std::io::Error>())
        .unwrap()
        .get_class(),
      Category::Syntax => SYNTAX_ERROR,
      Category::Data => "InvalidData",
      Category::Eof => "UnexpectedEof",
    }
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None // TODO: could be io error code
  }
}

#[cfg(feature = "url")]
impl JsErrorClass for url::ParseError {
  fn get_class(&self) -> &'static str {
    URI_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

#[cfg(feature = "tokio")]
impl<T: Send + Sync + 'static> JsErrorClass
  for tokio::sync::mpsc::error::SendError<T>
{
  fn get_class(&self) -> &'static str {
    GENERIC_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}

#[cfg(feature = "tokio")]
impl JsErrorClass for tokio::task::JoinError {
  fn get_class(&self) -> &'static str {
    GENERIC_ERROR
  }

  fn get_message(&self) -> Cow<'static, str> {
    self.to_string().into()
  }

  fn get_additional_properties(
    &self,
  ) -> Option<Vec<(Cow<'static, str>, Cow<'static, str>)>> {
    None
  }
}
