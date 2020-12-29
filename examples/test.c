int printf(char const *format, ...);

void empty() { }

void simple_arith() {
  (10 - 10/3) << 3 | (23+8*12) & 1024;
}

int constant_folding(float arg) {
  _Bool b;
  b = (_Bool) 1;
  if (b || (_Bool) 1) {
	  return (int) (arg * 1.0);
  } else {
	  return 0;
  }
}

void simple_arith_with_arg(int d) {
  (d > d/2) || (d >= 100) && (d < 99);
}

int factorial(int n)
{
	if (n <= 0)
		return 1;
	else return n*factorial(n-1);
}

int fibonacci(int a, int b, int n)
{
	if (n <= 0)
		return a;

	while (n > 0) {
		int t;
		t = b;
		b = a+b;
		a = t;
		n = n-1;
	}
	return b;
}

int main()
{
  printf("%d\n", fibonacci(factorial(1), factorial(2), factorial(5)));
  return 0;
}
