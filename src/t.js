function echo_0(...msg) {
  console.log(...msg);
}
function append_1(arr, item) {
  return arr.push(item);
}
function len_2(arr) {
  return arr.length;
}
function count_11(n_12) {
  let x_13 = 0;
  while (true) {
    echo_0(x_13);
    x_13 = x_13 + 1;
    if (x_13 > n_12) {
      return;
    }
  }
}

count_11(1);
count_11(5);
count_11(15);
