// The Swiss flag

//mutex a;
//mutex b;

void p1() {
  lock(a);
  lock(b);
  unlock(b);
  unlock(a);
}

void p2() {
  lock(b);
  lock(a);
  unlock(b);
  unlock(a);
}

void main() {
  spawn{p1()};
  spawn{p2()};
}
