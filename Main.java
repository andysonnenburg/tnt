import java.io.PrintStream;

public final class Main implements Runnable {
    public static final void main(String[] args) {
	run();
    }

    public final void run() {
	goto BEGIN;
	INCREMENT:
	switch (i) {
	case 0: iinc 2; break;
	}
	DECREMENT:
	switch (i) {
	case 0: iload_2; iconst_1; isub; istore_2; break;
	}
	PrintStream out = System.out;
	int i = 0;
	iconst_0; istore_2
	BEGIN:
    }
}