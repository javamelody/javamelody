package net.bull.javamelody;

import static org.junit.Assert.assertEquals;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.hibernate.ScrollMode;
import org.hibernate.cfg.Configuration;
import org.hibernate.impl.SessionFactoryImpl;
import org.hibernate.impl.StatelessSessionImpl;
import org.hibernate.jdbc.Batcher;
import org.junit.Before;
import org.junit.Test;

/**
 * Test unitaire de la classe HibernateBatcherFactory.
 * @author Emeric Vernat
 */
public class TestHibernateBatcherFactory {
	/** Initialisation. */
	@Before
	public void setUp() {
		Utils.initialize();
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testWithoutBatching() throws SQLException {
		doTest(false);
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testWithBatching() throws SQLException {
		doTest(true);
	}

	/** Test.
	 * @throws SQLException e */
	@Test
	public void testMonitoringDisabled() throws SQLException {
		Utils.setProperty(Parameter.DISABLED, "true");
		doTest(false);
		doTest(true);
	}

	private void doTest(boolean batching) throws SQLException {
		final Counter sqlCounter = JdbcWrapper.SINGLETON.getSqlCounter();
		sqlCounter.clear();
		final Configuration configuration = new Configuration();
		//		configuration.configure(getClass().getResource("/hibernate.cfg.xml"));
		configuration.setProperty("hibernate.connection.driver_class", "org.h2.Driver");
		configuration.setProperty("hibernate.connection.url", TestJdbcWrapper.H2_DATABASE_URL);
		configuration.setProperty("hibernate.dialect", "org.hibernate.dialect.H2Dialect");
		configuration.setProperty("hibernate.jdbc.factory_class",
				"net.bull.javamelody.HibernateBatcherFactory");

		if (batching) {
			configuration.setProperty("hibernate.jdbc.batch_size", "50");
		} else {
			configuration.setProperty("hibernate.jdbc.batch_size", "0");
		}
		final SessionFactoryImpl sessionFactory = (SessionFactoryImpl) configuration
				.buildSessionFactory();
		//		final Connection connection = statelessSession.connection();
		final StatelessSessionImpl statelessSession = (StatelessSessionImpl) sessionFactory
				.openStatelessSession();
		try {
			final Batcher batcher = statelessSession.getBatcher();
			final String sql = "select 1";
			batcher.prepareBatchCallableStatement(sql).close();
			batcher.prepareBatchStatement(sql).close();
			batcher.prepareCallableQueryStatement(sql, false, ScrollMode.FORWARD_ONLY).close();
			batcher.prepareCallableStatement(sql).close();
			batcher.prepareQueryStatement(sql, false, ScrollMode.FORWARD_ONLY).close();
			batcher.prepareSelectStatement(sql).close();
			batcher.prepareStatement(sql, false).close();
			batcher.prepareStatement(sql, new String[] {}).close();
			final PreparedStatement statement = batcher.prepareStatement(sql);
			statement.execute();
			statement.close();
		} finally {
			statelessSession.close();
		}
		if (JdbcWrapper.SINGLETON.isSqlMonitoringDisabled()) {
			assertEquals("requestsCount", 0L, sqlCounter.getRequestsCount());
		} else {
			assertEquals("requestsCount", 1L, sqlCounter.getRequestsCount());
			assertEquals("hits", 1L, sqlCounter.getRequests().get(0).getHits());
		}
	}
}
