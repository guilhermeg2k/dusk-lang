public class Quicksort {
    static void swap(int[] a, int i, int j) {
        int t = a[i];
        a[i] = a[j];
        a[j] = t;
    }

    static void quicksort(int[] a, int lo, int hi) {
        if (lo >= hi) return;

        int pivot = a[(lo + hi) >>> 1];
        int i = lo;
        int j = hi;

        while (i <= j) {
            while (a[i] < pivot) i++;
            while (a[j] > pivot) j--;
            if (i <= j) {
                swap(a, i, j);
                i++;
                j--;
            }
        }

        quicksort(a, lo, j);
        quicksort(a, i, hi);
    }

    public static void main(String[] args) {
        int n = 10000;
        int[] data = new int[n];
        for (int i = 0; i < n; i++) {
            data[i] = n - i;
        }

        quicksort(data, 0, n - 1);
        System.out.println(data[5000]);
    }
}
