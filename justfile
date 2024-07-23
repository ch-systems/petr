
alias t := test

test:
  CARGO_TERM_COLOR=always NO_COLOR=1 cargo test 

dtest:
  CARGO_TERM_COLOR=always NO_COLOR=1 cargo test --features=debug

update-expects:
  CARGO_TERM_COLOR=always NO_COLOR=1 UPDATE_EXPECT=1 cargo test
