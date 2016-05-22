/* utils */

let starts_with (first : string) (prefix : string) => {
  let prefix_length = String.length prefix;
  if (String.length first < prefix_length) {
    false
  } else {
    let comparison = String.compare
      (String.sub first 0 prefix_length)
      prefix;
    comparison == 0 /* return a bool */
  }
};

let assert_not a => assert (not a);

/* testing section */

let run_tests => {
  assert (starts_with "hi there" "hi");
  assert_not (starts_with "hi there" "hitmantop");
};

run_tests();
