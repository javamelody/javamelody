package net.bull.javamelody;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import org.hibernate.Interceptor;
import org.hibernate.ScrollMode;
import org.hibernate.cfg.Settings;
import org.hibernate.jdbc.Batcher;
import org.hibernate.jdbc.BatcherFactory;
import org.hibernate.jdbc.BatchingBatcher;
import org.hibernate.jdbc.ConnectionManager;
import org.hibernate.jdbc.NonBatchingBatcher;

/**
 * Alternative pour monitorer les requêtes sql.
 * Cette classe se configure avec la propriété hibernate "hibernate.jdbc.factory_class"
 *
 * Par exemple: hibernate.jdbc.factory_class=net.bull.javamelody.HibernateBatcherFactory
 *
 * @author Emeric Vernat
 */
public class HibernateBatcherFactory implements BatcherFactory {

	private static class HibernateBatchingBatcher extends BatchingBatcher {
		HibernateBatchingBatcher(ConnectionManager connectionManager, Interceptor interceptor) {
			super(connectionManager, interceptor);
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql) throws SQLException {
			// super.prepareStatement utilisera prepareStatement(String, boolean)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createPreparedStatementProxy(sql, super.prepareStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareBatchStatement(String sql) throws SQLException {
			// prepareBatchStatement utilisera prepareStatement(String, boolean)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createPreparedStatementProxy(sql, super.prepareBatchStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql, boolean getGeneratedKeys)
				throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareStatement(sql, getGeneratedKeys));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareSelectStatement(String sql) throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareSelectStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql, String[] columnNames)
				throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareStatement(sql, columnNames));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareQueryStatement(String sql, boolean scrollable,
				ScrollMode scrollMode) throws SQLException {
			return createPreparedStatementProxy(sql,
					super.prepareQueryStatement(sql, scrollable, scrollMode));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareCallableStatement(String sql) throws SQLException {
			return createCallableStatementProxy(sql, super.prepareCallableStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareCallableQueryStatement(String sql, boolean scrollable,
				ScrollMode scrollMode) throws SQLException {
			return createCallableStatementProxy(sql,
					super.prepareCallableQueryStatement(sql, scrollable, scrollMode));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareBatchCallableStatement(String sql) throws SQLException {
			// prepareBatchCallableStatement utilisera prepareCallableStatement(String)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createCallableStatementProxy(sql, super.prepareBatchCallableStatement(sql));
		}
	}

	private static class HibernateNonBatchingBatcher extends NonBatchingBatcher {
		HibernateNonBatchingBatcher(ConnectionManager connectionManager, Interceptor interceptor) {
			super(connectionManager, interceptor);
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql) throws SQLException {
			// super.prepareStatement utilisera prepareStatement(String, boolean)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createPreparedStatementProxy(sql, super.prepareStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareBatchStatement(String sql) throws SQLException {
			// prepareBatchStatement utilisera prepareStatement(String, boolean)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createPreparedStatementProxy(sql, super.prepareBatchStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareSelectStatement(String sql) throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareSelectStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql, boolean getGeneratedKeys)
				throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareStatement(sql, getGeneratedKeys));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareStatement(String sql, String[] columnNames)
				throws SQLException {
			return createPreparedStatementProxy(sql, super.prepareStatement(sql, columnNames));
		}

		/** {@inheritDoc} */
		@Override
		public PreparedStatement prepareQueryStatement(String sql, boolean scrollable,
				ScrollMode scrollMode) throws SQLException {
			return createPreparedStatementProxy(sql,
					super.prepareQueryStatement(sql, scrollable, scrollMode));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareBatchCallableStatement(String sql) throws SQLException {
			// prepareBatchCallableStatement utilisera prepareCallableStatement(String)
			// donc override a priori inutile mais on laisse createProxy vérifier
			return createCallableStatementProxy(sql, super.prepareBatchCallableStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareCallableStatement(String sql) throws SQLException {
			return createCallableStatementProxy(sql, super.prepareCallableStatement(sql));
		}

		/** {@inheritDoc} */
		@Override
		public CallableStatement prepareCallableQueryStatement(String sql, boolean scrollable,
				ScrollMode scrollMode) throws SQLException {
			return createCallableStatementProxy(sql,
					super.prepareCallableQueryStatement(sql, scrollable, scrollMode));
		}
	}

	static PreparedStatement createPreparedStatementProxy(String query, PreparedStatement statement) {
		return (PreparedStatement) JdbcWrapper.SINGLETON.createStatementProxy(query, statement);
	}

	static CallableStatement createCallableStatementProxy(String query, CallableStatement statement) {
		return (CallableStatement) JdbcWrapper.SINGLETON.createStatementProxy(query, statement);
	}

	/** {@inheritDoc} */
	@Override
	public Batcher createBatcher(ConnectionManager connectionManager, Interceptor interceptor) {
		final boolean sqlMonitoringDisabled = JdbcWrapper.SINGLETON.isSqlMonitoringDisabled();
		final Settings settings = connectionManager.getFactory().getSettings();
		if (sqlMonitoringDisabled) {
			if (settings.getJdbcBatchSize() == 0) {
				return new NonBatchingBatcher(connectionManager, interceptor);
			}
			return new BatchingBatcher(connectionManager, interceptor);
		}

		final Batcher result;
		if (settings.getJdbcBatchSize() == 0) {
			result = new HibernateNonBatchingBatcher(connectionManager, interceptor);
		} else {
			result = new HibernateBatchingBatcher(connectionManager, interceptor);
		}
		LOG.debug("hibernate batcher factory initialized");
		return result;
	}
}
