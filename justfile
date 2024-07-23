
alias t := test

test:
  CARGO_TERM_COLOR=always NO_COLOR=1 cargo test

update-expects:
  CARGO_TERM_COLOR=always NO_COLOR=1 UPDATE_EXPECT=1 cargo test

publish:
  cargo publish petr-parse &&
  cargo publish petr-fmt &&
  cargo publish petr-utils &&
  cargo publish petr-cli &&
  cargo publish petr-codegen &&
  cargo publish petr-ast &&
  cargo publish petr-typecheck &&
  cargo publish petr-bind &&
  cargo publish petr-resolve &&
  cargo publish petr-ir &&
  cargo publish petr-vm &&
  cargo publish petr-pkg &&
  cargo publish petr-profiling &&
  cargo publish petr-api &&
  cargo publish petr-playground

