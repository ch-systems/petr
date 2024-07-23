
alias t := test

test:
  CARGO_TERM_COLOR=always NO_COLOR=1 cargo test

update-expects:
  CARGO_TERM_COLOR=always NO_COLOR=1 UPDATE_EXPECT=1 cargo test

publish:
  cargo publish -p petr-utils
  cargo publish -p petr-ast
  cargo publish -p petr-profiling
  cargo publish -p petr-parse
  cargo publish -p petr-fmt
  cargo publish -p petr-bind
  cargo publish -p petr-resolve
  cargo publish -p petr-typecheck
  cargo publish -p petr-ir
  cargo publish -p petr-vm
  cargo publish -p petr-codegen
  cargo publish -p petr-pkg
  cargo publish -p petr-api
  cargo publish -p petr-playground
  cargo publish -p petr-cli
