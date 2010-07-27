package net.bull.javamelody;

import org.hibernate.cfg.Configuration;
import org.junit.Before;

/**
 * Test unitaire de la classe HibernateBatcherFactory.
 * @author Emeric Vernat
 */
public class TestHibernateBatcherFactory {
	/** Test. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	public void test() {
		final Configuration configuration = new Configuration();
		configuration.addFile("/hibernate.cfg.xml");
	}
}
