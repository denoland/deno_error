use deno_error::JsErrorClass;
use std::borrow::Cow;
use std::io::ErrorKind;

#[test]
fn test_properties() {
  #[derive(Debug, thiserror::Error, deno_error::JsError)]
  #[class(type)]
  pub enum SomeError {
    #[error("Failure")]
    Failure,
    #[class(inherit)]
    #[property("foo" = "1")]
    #[error(transparent)]
    Io(std::io::Error),
    #[class(type)]
    #[properties(inherit)]
    #[property("foo" = "1")]
    #[error(transparent)]
    Io2(std::io::Error),
    #[class(inherit)]
    #[properties(no_inherit)]
    #[error(transparent)]
    Io3(std::io::Error),
  }

  assert_eq!(SomeError::Failure.get_additional_properties(), []);
  assert_eq!(
    SomeError::Io(std::io::Error::new(ErrorKind::AddrInUse, "foo"))
      .get_additional_properties(),
    [
      (Cow::Borrowed("code"), Cow::Borrowed("EADDRINUSE")),
      (Cow::Borrowed("foo"), Cow::Borrowed("1"))
    ]
  );
  assert_eq!(
    SomeError::Io2(std::io::Error::new(ErrorKind::AddrInUse, "foo"))
      .get_additional_properties(),
    [
      (Cow::Borrowed("code"), Cow::Borrowed("EADDRINUSE")),
      (Cow::Borrowed("foo"), Cow::Borrowed("1"))
    ]
  );
  assert_eq!(
    SomeError::Io3(std::io::Error::new(ErrorKind::AddrInUse, "foo"))
      .get_additional_properties(),
    []
  );
}
