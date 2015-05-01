package example;

import java.math.BigInteger;

/**
 * Compute PI just to have something to do,
 * from http://rosettacode.org/wiki/Pi#Java
 */
public class Pi {
	private static final BigInteger TWO = BigInteger.valueOf(2);
	private static final BigInteger THREE = BigInteger.valueOf(3);
	private static final BigInteger FOUR = BigInteger.valueOf(4);
	private static final BigInteger SEVEN = BigInteger.valueOf(7);

	private BigInteger q = BigInteger.ONE;
	private BigInteger r = BigInteger.ZERO;
	private BigInteger t = BigInteger.ONE;
	private BigInteger k = BigInteger.ONE;
	private BigInteger n = BigInteger.valueOf(3);
	private BigInteger l = BigInteger.valueOf(3);

	/**
	 * Start.
	 */
	public void calcPiDigits() {
		BigInteger nn, nr;
		boolean first = true;
		while (true) {
			if (FOUR.multiply(q).add(r).subtract(t).compareTo(n.multiply(t)) == -1) {
				System.out.print(n);
				if (first) {
					System.out.print(".");
					first = false;
				}
				nr = BigInteger.TEN.multiply(r.subtract(n.multiply(t)));
				n = BigInteger.TEN.multiply(THREE.multiply(q).add(r)).divide(t)
						.subtract(BigInteger.TEN.multiply(n));
				q = q.multiply(BigInteger.TEN);
				r = nr;
				System.out.flush();
			} else {
				nr = TWO.multiply(q).add(r).multiply(l);
				nn = q.multiply(SEVEN.multiply(k)).add(TWO).add(r.multiply(l))
						.divide(t.multiply(l));
				q = q.multiply(k);
				t = t.multiply(l);
				l = l.add(TWO);
				k = k.add(BigInteger.ONE);
				n = nn;
				r = nr;
			}
		}
	}
}
