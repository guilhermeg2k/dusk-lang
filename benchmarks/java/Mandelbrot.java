public class Mandelbrot {
    public static void main(String[] args) {
        int w = 800;
        int h = 800;
        int maxIter = 1000;

        long sum = 0;
        for (int py = 0; py < h; py++) {
            double cy = -1.0 + ((double) py / h) * 2.0;
            for (int px = 0; px < w; px++) {
                double cx = -2.5 + ((double) px / w) * 3.5;

                double zx = 0.0;
                double zy = 0.0;
                double zx2 = 0.0;
                double zy2 = 0.0;
                int iter = 0;

                while (zx2 + zy2 <= 4.0 && iter < maxIter) {
                    zy = 2.0 * zx * zy + cy;
                    zx = zx2 - zy2 + cx;
                    zx2 = zx * zx;
                    zy2 = zy * zy;
                    iter++;
                }

                sum += iter;
            }
        }

        System.out.println(sum);
    }
}
