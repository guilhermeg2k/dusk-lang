public class Particles {
    static final class Particle {
        double x, y, vx, vy;

        Particle(double x, double y, double vx, double vy) {
            this.x = x;
            this.y = y;
            this.vx = vx;
            this.vy = vy;
        }
    }

    public static void main(String[] args) {
        int n = 100000;
        Particle[] particles = new Particle[n];
        for (int i = 0; i < n; i++) {
            particles[i] = new Particle(i, i * 2.0, 0.5, -0.5);
        }

        for (int step = 0; step < 100; step++) {
            for (int i = 0; i < n; i++) {
                Particle p = particles[i];
                p.x += p.vx;
                p.y += p.vy;
            }
        }

        System.out.println(particles[49999].x);
    }
}
