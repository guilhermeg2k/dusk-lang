public class Primes {
    public static void main(String[] args) {
        int limit = 100000;
        boolean[] sieve = new boolean[limit + 1];
        for (int i = 2; i <= limit; i++) {
            sieve[i] = true;
        }

        for (int p = 2; (long) p * p <= limit; p++) {
            if (sieve[p]) {
                for (int m = p * p; m <= limit; m += p) {
                    sieve[m] = false;
                }
            }
        }

        int count = 0;
        for (int k = 2; k <= limit; k++) {
            if (sieve[k]) count++;
        }

        System.out.println(count);
    }
}
