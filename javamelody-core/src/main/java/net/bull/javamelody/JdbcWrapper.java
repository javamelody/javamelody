/*
 * Copyright 2008-2017 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.io.Serializable;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import javax.naming.Context;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;
import net.bull.javamelody.internal.model.ConnectionInformations;
import net.bull.javamelody.internal.model.Counter;

/**
 * Cette classe est utile pour construire des proxy de {@link DataSource}s ou de {@link Connection}s jdbc.<br>
 * Et notamment elle rebinde dans l'annuaire JNDI la dataSource jdbc en la remplaçant
 * par un proxy de monitoring.
 * @author Emeric Vernat
 */
public final class JdbcWrapper {
	/**
	 * Instance singleton de JdbcWrapper (ici on ne connaît pas le ServletContext).
	 */
	public static final JdbcWrapper SINGLETON = new JdbcWrapper(
			new Counter(Counter.SQL_COUNTER_NAME, "db.png"));

	// au lieu d'utiliser int avec des synchronized partout, on utilise AtomicInteger
	static final AtomicInteger ACTIVE_CONNECTION_COUNT = new AtomicInteger();
	static final AtomicInteger USED_CONNECTION_COUNT = new AtomicInteger();
	static final AtomicLong TRANSACTION_COUNT = new AtomicLong();
	static final AtomicInteger ACTIVE_THREAD_COUNT = new AtomicInteger();
	static final AtomicInteger RUNNING_BUILD_COUNT = new AtomicInteger();
	static final AtomicInteger BUILD_QUEUE_LENGTH = new AtomicInteger();
	static final Map<Integer, ConnectionInformations> USED_CONNECTION_INFORMATIONS = new ConcurrentHashMap<Integer, ConnectionInformations>();

	private static final int MAX_USED_CONNECTION_INFORMATIONS = 500;

	// Cette variable sqlCounter conserve un état qui est global au filtre et à l'application (donc thread-safe).
	private final Counter sqlCounter;
	private ServletContext servletContext;
	private boolean connectionInformationsEnabled;
	private boolean jboss;
	private boolean glassfish;
	private boolean weblogic;

	static final class ConnectionInformationsComparator
			implements Comparator<ConnectionInformations>, Serializable {
		private static final long serialVersionUID = 1L;

		/** {@inheritDoc} */
		@Override
		public int compare(ConnectionInformations connection1, ConnectionInformations connection2) {
			return connection1.getOpeningDate().compareTo(connection2.getOpeningDate());
		}
	}

	/**
	 * Handler de proxy d'un {@link Statement} jdbc.
	 */
	private class StatementInvocationHandler implements InvocationHandler {
		// Rq : dans les proxy de DataSource, Connection et Statement,
		// si la méthode appelée est java.sql.Wrapper.unwrap
		// on invoque toujours unwrap sur l'objet initial pour qu'il retourne lui-même
		// ou son objet wrappé. Par exemple, l'appel de unwrap sur le proxy d'un Statement
		// retournera le Statement initial du serveur ou même du driver bdd (OracleStatement...)
		// sans notre proxy pour pouvoir appeler les méthodes non standard du driver par ex.
		private String requestName;
		private final Statement statement;

		StatementInvocationHandler(String query, Statement statement) {
			super();
			assert statement != null;

			this.requestName = query;
			this.statement = statement;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			// performance : on évite method.invoke pour equals & hashCode
			final String methodName = method.getName();
			if (isEqualsMethod(methodName, args)) {
				return statement.equals(args[0]);
			} else if (isHashCodeMethod(methodName, args)) {
				return statement.hashCode();
			} else if (methodName.startsWith("execute")) {
				if (isFirstArgAString(args)) {
					// la méthode est du type executeQuery(String), executeUpdate(String),
					// executeUpdate(String, ...) ou execute(String sql),
					// alors la requête sql est le premier argument (et pas query)
					requestName = (String) args[0];
				} else if ("executeBatch".equals(methodName)
						&& !requestName.startsWith("/* BATCH */ ")) {
					// if executeBatch, add a prefix in the request name to explain that
					// 1 batch "hit" is equivalent to several exec of the request in the db
					requestName = "/* BATCH */ " + requestName;
				}

				// si on n'a pas trouvé la requête, on prend "null"
				requestName = String.valueOf(requestName);

				return doExecute(requestName, statement, method, args);
			} else if ("addBatch".equals(methodName) && isFirstArgAString(args)) {
				// Bien que déconseillée la méthode est addBatch(String),
				// la requête sql est alors le premier argument
				// (elle sera utilisée lors de l'appel à executeBatch())

				// Rq : on ne conserve que la dernière requête de addBatch.
				// Rq : si addBatch(String) est appelée, puis que executeUpdate(String)
				// la requête du batch est correctement ignorée ci-dessus.
				// Rq : si connection.prepareStatement(String).addBatch(String) puis executeUpdate()
				// sont appelées (et pas executeBatch()) alors la requête conservée est
				// faussement celle du batch mais l'application cloche grave.
				requestName = (String) args[0];
			}

			// ce n'est pas une méthode executeXxx du Statement
			return method.invoke(statement, args);
		}

		private boolean isFirstArgAString(Object[] args) {
			return args != null && args.length > 0 && args[0] instanceof String;
		}
	}

	/**
	 * Handler de proxy d'une {@link Connection} jdbc.
	 */
	private class ConnectionInvocationHandler implements InvocationHandler {
		private final Connection connection;
		private boolean alreadyClosed;

		ConnectionInvocationHandler(Connection connection) {
			super();
			assert connection != null;
			this.connection = connection;
		}

		void init() {
			// on limite la taille pour éviter une éventuelle saturation mémoire
			if (isConnectionInformationsEnabled()
					&& USED_CONNECTION_INFORMATIONS.size() < MAX_USED_CONNECTION_INFORMATIONS) {
				USED_CONNECTION_INFORMATIONS.put(
						ConnectionInformations.getUniqueIdOfConnection(connection),
						new ConnectionInformations());
			}
			USED_CONNECTION_COUNT.incrementAndGet();
			TRANSACTION_COUNT.incrementAndGet();
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			// performance : on évite method.invoke pour equals & hashCode
			final String methodName = method.getName();
			if (isEqualsMethod(methodName, args)) {
				return areConnectionsEquals(args[0]);
			} else if (isHashCodeMethod(methodName, args)) {
				return connection.hashCode();
			}
			try {
				Object result = method.invoke(connection, args);
				if (result instanceof Statement) {
					final String requestName;
					if ("prepareStatement".equals(methodName) || "prepareCall".equals(methodName)) {
						// la méthode est du type prepareStatement(String) ou prepareCall(String),
						// alors la requête sql est le premier argument
						requestName = (String) args[0];
					} else {
						requestName = null;
					}
					result = createStatementProxy(requestName, (Statement) result);
				}
				return result;
			} finally {
				if ("close".equals(methodName) && !alreadyClosed) {
					USED_CONNECTION_COUNT.decrementAndGet();
					USED_CONNECTION_INFORMATIONS
							.remove(ConnectionInformations.getUniqueIdOfConnection(connection));
					alreadyClosed = true;
				}
			}
		}

		private boolean areConnectionsEquals(Object object) {
			// Special case if what we're being passed is one of our proxies (specifically a connection proxy)
			// This way the equals call is truely transparent for our proxies (cf issue 78)
			if (Proxy.isProxyClass(object.getClass())) {
				final InvocationHandler invocationHandler = Proxy.getInvocationHandler(object);
				if (invocationHandler instanceof DelegatingInvocationHandler) {
					final DelegatingInvocationHandler d = (DelegatingInvocationHandler) invocationHandler;
					if (d.getDelegate() instanceof ConnectionInvocationHandler) {
						final ConnectionInvocationHandler c = (ConnectionInvocationHandler) d
								.getDelegate();
						return connection.equals(c.connection);
					}
				}
			}
			return connection.equals(object);
		}
	}

	private static class ConnectionManagerInvocationHandler
			extends AbstractInvocationHandler<Object> {
		// classe sérialisable pour glassfish v2.1.1, issue 229: Exception in NamingManagerImpl copyMutableObject()
		private static final long serialVersionUID = 1L;

		ConnectionManagerInvocationHandler(Object javaxConnectionManager) {
			super(javaxConnectionManager);
		}

		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			final Object result = method.invoke(getProxiedObject(), args);
			if (result instanceof Connection) {
				SINGLETON.createConnectionProxyOrRewrapIfJBossOrGlassfish((Connection) result);
			}
			return result;
		}
	}

	private abstract static class AbstractInvocationHandler<T>
			implements InvocationHandler, Serializable {
		private static final long serialVersionUID = 1L;

		@SuppressWarnings("all")
		private final T proxiedObject;

		AbstractInvocationHandler(T proxiedObject) {
			super();
			this.proxiedObject = proxiedObject;
		}

		T getProxiedObject() {
			return proxiedObject;
		}
	}

	// ce handler désencapsule les InvocationTargetException des proxy
	private static class DelegatingInvocationHandler implements InvocationHandler, Serializable {
		// classe sérialisable pour MonitoringProxy
		private static final long serialVersionUID = 7515240588169084785L;
		@SuppressWarnings("all")
		private final InvocationHandler delegate;

		DelegatingInvocationHandler(InvocationHandler delegate) {
			super();
			this.delegate = delegate;
		}

		InvocationHandler getDelegate() {
			return delegate;
		}

		/** {@inheritDoc} */
		@Override
		public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
			try {
				return delegate.invoke(proxy, method, args);
			} catch (final InvocationTargetException e) {
				if (e.getTargetException() != null) {
					throw e.getTargetException();
				}
				throw e;
			}
		}
	}

	private JdbcWrapper(Counter sqlCounter) {
		super();
		assert sqlCounter != null;
		this.sqlCounter = sqlCounter;
		// servletContext reste null pour l'instant
		this.servletContext = null;
		connectionInformationsEnabled = Parameters.isSystemActionsEnabled()
				&& !Parameters.isNoDatabase();
	}

	void initServletContext(ServletContext context) {
		assert context != null;
		this.servletContext = context;
		final String serverInfo = servletContext.getServerInfo();
		jboss = serverInfo.contains("JBoss") || serverInfo.contains("WildFly");
		glassfish = serverInfo.contains("GlassFish")
				|| serverInfo.contains("Sun Java System Application Server")
				|| serverInfo.contains("Payara");
		weblogic = serverInfo.contains("WebLogic");
		connectionInformationsEnabled = Parameters.isSystemActionsEnabled()
				&& !Parameters.isNoDatabase();
	}

	public static int getUsedConnectionCount() {
		return USED_CONNECTION_COUNT.get();
	}

	public static int getActiveConnectionCount() {
		return ACTIVE_CONNECTION_COUNT.get();
	}

	public static long getTransactionCount() {
		return TRANSACTION_COUNT.get();
	}

	public static int getActiveThreadCount() {
		return ACTIVE_THREAD_COUNT.get();
	}

	public static int getRunningBuildCount() {
		return RUNNING_BUILD_COUNT.get();
	}

	public static int getBuildQueueLength() {
		return BUILD_QUEUE_LENGTH.get();
	}

	public static List<ConnectionInformations> getConnectionInformationsList() {
		final List<ConnectionInformations> result = new ArrayList<ConnectionInformations>(
				USED_CONNECTION_INFORMATIONS.values());
		Collections.sort(result, new ConnectionInformationsComparator());
		return Collections.unmodifiableList(result);
	}

	public Counter getSqlCounter() {
		return sqlCounter;
	}

	boolean isConnectionInformationsEnabled() {
		return connectionInformationsEnabled;
	}

	public static int getMaxConnectionCount() {
		return JdbcWrapperHelper.getMaxConnectionCount();
	}

	public static Map<String, Map<String, Object>> getBasicDataSourceProperties() {
		return JdbcWrapperHelper.getBasicDataSourceProperties();
	}

	public static Map<String, DataSource> getJndiAndSpringDataSources() throws NamingException {
		return JdbcWrapperHelper.getJndiAndSpringDataSources();
	}

	/**
	 * Enregistre une {@link DataSource} ne venant pas de JNDI.
	 * @param name String
	 * @param dataSource DataSource
	 */
	public static void registerSpringDataSource(String name, DataSource dataSource) {
		JdbcWrapperHelper.registerSpringDataSource(name, dataSource);
	}

	Object doExecute(String requestName, Statement statement, Method method, Object[] args)
			throws IllegalAccessException, InvocationTargetException {
		assert requestName != null;
		assert statement != null;
		assert method != null;

		// on ignore les requêtes explain exécutées par DatabaseInformations
		if (!sqlCounter.isDisplayed() || requestName.startsWith("explain ")) {
			ACTIVE_CONNECTION_COUNT.incrementAndGet();
			try {
				return method.invoke(statement, args);
			} finally {
				ACTIVE_CONNECTION_COUNT.decrementAndGet();
			}
		}

		final long start = System.currentTimeMillis();
		boolean systemError = true;
		try {
			ACTIVE_CONNECTION_COUNT.incrementAndGet();

			// note perf: selon un paramètre current-sql(/requests)-disabled,
			// on pourrait ici ne pas binder un nouveau contexte à chaque requête sql
			sqlCounter.bindContext(requestName, requestName, null, -1);

			final Object result = method.invoke(statement, args);
			systemError = false;
			return result;
		} catch (final InvocationTargetException e) {
			if (e.getCause() instanceof SQLException) {
				final int errorCode = ((SQLException) e.getCause()).getErrorCode();
				if (errorCode >= 20000 && errorCode < 30000) {
					// Dans Oracle par exemple, les erreurs 20000 à 30000 sont standardisées
					// comme étant des erreurs lancées par l'application dans des procédures stockées
					// pour être traitées comme des erreurs de saisies ou comme des règles de gestion.
					// Ce ne sont donc pas des erreurs systèmes.
					systemError = false;
				}
			}
			throw e;
		} finally {
			// Rq : on n'utilise pas la création du statement et l'appel à la méthode close du statement
			// comme début et fin d'une connexion active, car en fonction de l'application
			// la méthode close du statement peut ne jamais être appelée
			// (par exemple, seule la méthode close de la connection peut être appelée ce qui ferme aussi le statement)
			// Rq : pas de temps cpu pour les requêtes sql car c'est 0 ou quasiment 0
			ACTIVE_CONNECTION_COUNT.decrementAndGet();
			final long duration = Math.max(System.currentTimeMillis() - start, 0);
			sqlCounter.addRequest(requestName, duration, -1, systemError, -1);
		}
	}

	boolean rebindDataSources() {
		boolean ok;
		// on cherche une datasource avec InitialContext pour afficher nom et version bdd + nom et version driver jdbc
		// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
		try {
			final boolean rewrapDataSources = Parameter.REWRAP_DATASOURCES.getValueAsBoolean();
			if (rewrapDataSources || Parameter.DATASOURCES.getValue() != null) {
				// on annule le rebinding ou rewrapping éventuellement faits avant par SessionListener
				// si datasources ou rewrap-datasources est défini dans le filter
				stop();
			}
			final Map<String, DataSource> jndiDataSources = JdbcWrapperHelper.getJndiDataSources();
			LOG.debug("datasources found in JNDI: " + jndiDataSources.keySet());
			for (final Map.Entry<String, DataSource> entry : jndiDataSources.entrySet()) {
				final String jndiName = entry.getKey();
				final DataSource dataSource = entry.getValue();
				try {
					if (rewrapDataSources || isServerNeedsRewrap(jndiName)) {
						rewrapDataSource(jndiName, dataSource);
						JdbcWrapperHelper.registerRewrappedDataSource(jndiName, dataSource);
					} else if (!isProxyAlready(dataSource)) {
						// si dataSource est déjà un proxy, il ne faut pas faire un proxy d'un proxy ni un rebinding
						final DataSource dataSourceProxy = createDataSourceProxy(jndiName,
								dataSource);
						JdbcWrapperHelper.rebindDataSource(servletContext, jndiName, dataSource,
								dataSourceProxy);
						LOG.debug("datasource rebinded: " + jndiName + " from class "
								+ dataSource.getClass().getName() + " to class "
								+ dataSourceProxy.getClass().getName());
					}
				} catch (final Throwable t) { // NOPMD
					// ça n'a pas marché, tant pis pour celle-ci qui semble invalide, mais continuons avec les autres
					LOG.debug("rebinding datasource " + jndiName + " failed, skipping it", t);
				}
			}
			ok = true;
		} catch (final Throwable t) { // NOPMD
			// ça n'a pas marché, tant pis
			LOG.debug("rebinding datasources failed, skipping", t);
			ok = false;
		}
		return ok;
	}

	private void rewrapDataSource(String jndiName, DataSource dataSource)
			throws IllegalAccessException {
		final String dataSourceClassName = dataSource.getClass().getName();
		LOG.debug("Datasource needs rewrap: " + jndiName + " of class " + dataSourceClassName);
		final String dataSourceRewrappedMessage = "Datasource rewrapped: " + jndiName;
		if (isJBossOrGlassfishDataSource(dataSourceClassName)) {
			// JBOSS: le rebind de la datasource dans le JNDI JBoss est possible mais ne
			// fonctionne pas (car tous les lookup renverraient alors une instance de
			// MarshalledValuePair ou une instance javax.naming.Reference selon comment cela
			// est fait), donc on modifie directement l'instance de WrapperDataSource déjà
			// présente dans le JNDI.
			// GLASSFISH: le contexte JNDI commençant par "java:" est en lecture seule
			// dans glassfish (comme dit dans la spec et comme implémenté dans
			// http://kickjava.com/src/com/sun/enterprise/naming/java/javaURLContext.java.htm),
			// donc on modifie directement l'instance de DataSource40 déjà présente dans le
			// JNDI.
			// Par "chance", la classe org.jboss.resource.adapter.jdbc.WrapperDataSource et
			// la super-classe de com.sun.gjc.spi.jdbc40.DataSource40 contiennent toutes les
			// deux un attribut de nom "cm" et de type javax.resource.spi.ConnectionManager
			// dont on veut faire un proxy.
			Object javaxConnectionManager = JdbcWrapperHelper.getFieldValue(dataSource, "cm");
			javaxConnectionManager = createJavaxConnectionManagerProxy(javaxConnectionManager);
			JdbcWrapperHelper.setFieldValue(dataSource, "cm", javaxConnectionManager);
			LOG.debug(dataSourceRewrappedMessage);
		} else if (isWildfly9DataSource(dataSourceClassName)) {
			Object delegateDataSource = JdbcWrapperHelper.getFieldValue(dataSource, "delegate");
			delegateDataSource = createDataSourceProxy((DataSource) delegateDataSource);
			JdbcWrapperHelper.setFieldValue(dataSource, "delegate", delegateDataSource);
			LOG.debug(dataSourceRewrappedMessage);
		} else if (weblogic
				&& "weblogic.jdbc.common.internal.RmiDataSource".equals(dataSourceClassName)) {
			// WEBLOGIC: le contexte JNDI est en lecture seule donc on modifie directement
			// l'instance de RmiDataSource déjà présente dans le JNDI.
			rewrapWebLogicDataSource(dataSource);
			LOG.debug(dataSourceRewrappedMessage);
		} else if (isDbcpDataSource(dataSourceClassName)) {
			// JIRA dans Tomcat: la dataSource a déjà été mise en cache par org.ofbiz.core.entity.transaction.JNDIFactory
			// à l'initialisation de com.atlassian.jira.startup.JiraStartupChecklistContextListener
			// donc on modifie directement l'instance de BasicDataSource déjà présente dans le JNDI.
			// Et dans certains JIRA la datasource est bien une instance de org.apache.commons.dbcp.BasicDataSource
			// cf http://groups.google.com/group/javamelody/browse_thread/thread/da8336b908f1e3bd/6cf3048f1f11866e?show_docid=6cf3048f1f11866e

			// et aussi rewrap pour tomee/openejb (cf issue 104),
			rewrapBasicDataSource(dataSource);
			LOG.debug(dataSourceRewrappedMessage);
		} else if ("org.apache.openejb.resource.jdbc.managed.local.ManagedDataSource"
				.equals(dataSourceClassName)) {
			// rewrap pour tomee/openejb plus récents (cf issue 104),
			rewrapTomEEDataSource(dataSource);
			LOG.debug(dataSourceRewrappedMessage);
		} else {
			LOG.info("Datasource can't be rewrapped: " + jndiName + " of class "
					+ dataSourceClassName);
		}
	}

	private boolean isServerNeedsRewrap(String jndiName) {
		return glassfish || jboss || weblogic || jndiName.contains("openejb");
	}

	private boolean isDbcpDataSource(String dataSourceClassName) {
		return "org.apache.tomcat.dbcp.dbcp.BasicDataSource".equals(dataSourceClassName)
				|| "org.apache.tomcat.dbcp.dbcp2.BasicDataSource".equals(dataSourceClassName)
				|| "org.apache.commons.dbcp.BasicDataSource".equals(dataSourceClassName)
				|| "org.apache.commons.dbcp2.BasicDataSource".equals(dataSourceClassName)
				|| "org.apache.openejb.resource.jdbc.BasicManagedDataSource"
						.equals(dataSourceClassName)
				|| "org.apache.openejb.resource.jdbc.BasicDataSource".equals(dataSourceClassName);
	}

	private boolean isJBossOrGlassfishDataSource(String dataSourceClassName) {
		return jboss
				&& "org.jboss.resource.adapter.jdbc.WrapperDataSource".equals(dataSourceClassName)
				|| jboss && "org.jboss.jca.adapters.jdbc.WrapperDataSource"
						.equals(dataSourceClassName)
				|| glassfish && "com.sun.gjc.spi.jdbc40.DataSource40".equals(dataSourceClassName);
	}

	private boolean isWildfly9DataSource(String dataSourceClassName) {
		return jboss && "org.jboss.as.connector.subsystems.datasources.WildFlyDataSource"
				.equals(dataSourceClassName);
	}

	private void rewrapWebLogicDataSource(DataSource dataSource) throws IllegalAccessException {
		Object jdbcCtx = JdbcWrapperHelper.getFieldValue(dataSource, "jdbcCtx");
		if (jdbcCtx != null) {
			jdbcCtx = createContextProxy((Context) jdbcCtx);
			JdbcWrapperHelper.setFieldValue(dataSource, "jdbcCtx", jdbcCtx);
		}
		Object driverInstance = JdbcWrapperHelper.getFieldValue(dataSource, "driverInstance");
		if (driverInstance != null) {
			driverInstance = createDriverProxy((Driver) driverInstance);
			JdbcWrapperHelper.setFieldValue(dataSource, "driverInstance", driverInstance);
		}
	}

	private void rewrapBasicDataSource(DataSource dataSource) throws IllegalAccessException {
		// on récupère une connection avant de la refermer,
		// car sinon la datasource interne n'est pas encore créée
		// et le rewrap ne peut pas fonctionner
		try {
			dataSource.getConnection().close();
		} catch (final Exception e) {
			LOG.debug(e.toString());
			// ce n'est pas grave s'il y a une exception, par exemple parce que la base n'est pas disponible,
			// car l'essentiel est de créer la datasource
		}
		Object innerDataSource = JdbcWrapperHelper.getFieldValue(dataSource, "dataSource");
		if (innerDataSource != null) {
			innerDataSource = createDataSourceProxy((DataSource) innerDataSource);
			JdbcWrapperHelper.setFieldValue(dataSource, "dataSource", innerDataSource);
		}
	}

	private void rewrapTomEEDataSource(DataSource dataSource) throws IllegalAccessException {
		// on récupère une connection avant de la refermer,
		// car sinon la datasource interne n'est pas encore créée
		// et le rewrap ne peut pas fonctionner
		try {
			dataSource.getConnection().close();
		} catch (final Exception e) {
			LOG.debug(e.toString());
			// ce n'est pas grave s'il y a une exception, par exemple parce que la base n'est pas disponible,
			// car l'essentiel est de créer la datasource
		}
		Object innerDataSource = JdbcWrapperHelper.getFieldValue(dataSource, "delegate");
		if (innerDataSource != null) {
			innerDataSource = createDataSourceProxy((DataSource) innerDataSource);
			JdbcWrapperHelper.setFieldValue(dataSource, "delegate", innerDataSource);
		}
	}

	boolean stop() {
		boolean ok;
		try {
			JdbcWrapperHelper.rebindInitialDataSources(servletContext);

			// si jboss, glassfish ou weblogic avec datasource, on désencapsule aussi les objets wrappés
			final Map<String, DataSource> rewrappedDataSources = JdbcWrapperHelper
					.getRewrappedDataSources();
			for (final Map.Entry<String, DataSource> entry : rewrappedDataSources.entrySet()) {
				final String jndiName = entry.getKey();
				final DataSource dataSource = entry.getValue();
				unwrapDataSource(jndiName, dataSource);
			}
			rewrappedDataSources.clear();

			JdbcWrapperHelper.clearProxyCache();

			ok = true;
		} catch (final Throwable t) { // NOPMD
			// ça n'a pas marché, tant pis
			LOG.debug("rebinding initial datasources failed, skipping", t);
			ok = false;
		}
		return ok;
	}

	private void unwrapDataSource(String jndiName, DataSource dataSource)
			throws IllegalAccessException {
		final String dataSourceClassName = dataSource.getClass().getName();
		LOG.debug("Datasource needs unwrap: " + jndiName + " of class " + dataSourceClassName);
		final String dataSourceUnwrappedMessage = "Datasource unwrapped: " + jndiName;
		if (isJBossOrGlassfishDataSource(dataSourceClassName)) {
			unwrap(dataSource, "cm", dataSourceUnwrappedMessage);
		} else if (isWildfly9DataSource(dataSourceClassName)) {
			unwrap(dataSource, "delegate", dataSourceUnwrappedMessage);
		} else if (weblogic
				&& "weblogic.jdbc.common.internal.RmiDataSource".equals(dataSourceClassName)) {
			unwrap(dataSource, "jdbcCtx", dataSourceUnwrappedMessage);
			unwrap(dataSource, "driverInstance", dataSourceUnwrappedMessage);
		} else if (isDbcpDataSource(dataSourceClassName)) {
			unwrap(dataSource, "dataSource", dataSourceUnwrappedMessage);
		}
	}

	private void unwrap(Object parentObject, String fieldName, String unwrappedMessage)
			throws IllegalAccessException {
		final Object proxy = JdbcWrapperHelper.getFieldValue(parentObject, fieldName);
		if (Proxy.isProxyClass(proxy.getClass())) {
			InvocationHandler invocationHandler = Proxy.getInvocationHandler(proxy);
			if (invocationHandler instanceof DelegatingInvocationHandler) {
				invocationHandler = ((DelegatingInvocationHandler) invocationHandler).getDelegate();
				if (invocationHandler instanceof AbstractInvocationHandler) {
					final Object proxiedObject = ((AbstractInvocationHandler<?>) invocationHandler)
							.getProxiedObject();
					JdbcWrapperHelper.setFieldValue(parentObject, fieldName, proxiedObject);
					LOG.debug(unwrappedMessage);
				}
			}
		}
	}

	Context createContextProxy(final Context context) {
		assert context != null;
		final InvocationHandler invocationHandler = new AbstractInvocationHandler<Context>(
				context) {
			private static final long serialVersionUID = 1L;

			/** {@inheritDoc} */
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				Object result = method.invoke(context, args);
				if (result instanceof DataSource) {
					result = createDataSourceProxy((DataSource) result);
				}
				return result;
			}
		};
		return createProxy(context, invocationHandler);
	}

	// pour weblogic
	private Driver createDriverProxy(final Driver driver) {
		assert driver != null;
		final InvocationHandler invocationHandler = new AbstractInvocationHandler<Driver>(driver) {
			private static final long serialVersionUID = 1L;

			/** {@inheritDoc} */
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				Object result = method.invoke(driver, args);
				if (result instanceof Connection) {
					result = createConnectionProxy((Connection) result);
				}
				return result;
			}
		};
		return createProxy(driver, invocationHandler);
	}

	// pour jboss ou glassfish
	private Object createJavaxConnectionManagerProxy(Object javaxConnectionManager) {
		assert javaxConnectionManager != null;
		final InvocationHandler invocationHandler = new ConnectionManagerInvocationHandler(
				javaxConnectionManager);
		return createProxy(javaxConnectionManager, invocationHandler);
	}

	void rewrapConnection(Connection connection) throws IllegalAccessException {
		assert connection != null;
		if (jboss && connection.getClass().getSimpleName().startsWith("WrappedConnection")) {
			// pour jboss,
			// result instance de WrappedConnectionJDK6 ou WrappedConnectionJDK5
			// (attribut "mc" sur classe parente)
			final Object baseWrapperManagedConnection = JdbcWrapperHelper.getFieldValue(connection,
					"mc");
			final String conFieldName = "con";
			Connection con = (Connection) JdbcWrapperHelper
					.getFieldValue(baseWrapperManagedConnection, conFieldName);
			// on teste isProxyAlready ici pour raison de perf
			if (!isProxyAlready(con)) {
				con = createConnectionProxy(con);
				JdbcWrapperHelper.setFieldValue(baseWrapperManagedConnection, conFieldName, con);
			}
		} else if (glassfish && ("com.sun.gjc.spi.jdbc40.ConnectionHolder40"
				.equals(connection.getClass().getName())
				|| "com.sun.gjc.spi.jdbc40.ConnectionWrapper40"
						.equals(connection.getClass().getName()))) {
			// pour glassfish,
			// result instance de com.sun.gjc.spi.jdbc40.ConnectionHolder40
			// ou com.sun.gjc.spi.jdbc40.ConnectionWrapper40 selon message dans users' group
			// (attribut "con" sur classe parente)
			final String conFieldName = "con";
			Connection con = (Connection) JdbcWrapperHelper.getFieldValue(connection, conFieldName);
			// on teste isProxyAlready ici pour raison de perf
			if (!isProxyAlready(con)) {
				con = createConnectionProxy(con);
				JdbcWrapperHelper.setFieldValue(connection, conFieldName, con);
			}
		}
	}

	/**
	 * Crée un proxy d'une {@link DataSource} jdbc.
	 * @param dataSource DataSource
	 * @return DataSource
	 */
	public DataSource createDataSourceProxy(DataSource dataSource) {
		return createDataSourceProxy(null, dataSource);
	}

	/**
	 * Crée un proxy d'une {@link DataSource} jdbc.
	 * @param name String
	 * @param dataSource DataSource
	 * @return DataSource
	 */
	public DataSource createDataSourceProxy(String name, final DataSource dataSource) {
		assert dataSource != null;
		JdbcWrapperHelper.pullDataSourceProperties(name, dataSource);
		final InvocationHandler invocationHandler = new AbstractInvocationHandler<DataSource>(
				dataSource) {
			private static final long serialVersionUID = 1L;

			/** {@inheritDoc} */
			@Override
			public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
				Object result = method.invoke(dataSource, args);
				if (result instanceof Connection) {
					result = createConnectionProxy((Connection) result);
				}
				return result;
			}
		};
		return createProxy(dataSource, invocationHandler);
	}

	Connection createConnectionProxyOrRewrapIfJBossOrGlassfish(Connection connection)
			throws IllegalAccessException {
		if (jboss || glassfish) {
			rewrapConnection(connection);
			return connection;
		}
		return createConnectionProxy(connection);
	}

	/**
	 * Crée un proxy d'une {@link Connection} jdbc.
	 * @param connection Connection
	 * @return Connection
	 */
	public Connection createConnectionProxy(Connection connection) {
		assert connection != null;
		// même si le counter sql n'est pas affiché on crée un proxy de la connexion
		// pour avoir les graphiques USED_CONNECTION_COUNT et ACTIVE_CONNECTION_COUNT (cf issue 160)
		if (isMonitoringDisabled()) {
			return connection;
		}
		final ConnectionInvocationHandler invocationHandler = new ConnectionInvocationHandler(
				connection);
		final Connection result = createProxy(connection, invocationHandler);
		if (result != connection) {
			invocationHandler.init();
		}
		return result;
	}

	boolean isSqlMonitoringDisabled() {
		return isMonitoringDisabled() || !sqlCounter.isDisplayed();
	}

	private static boolean isMonitoringDisabled() {
		// on doit réévaluer ici le paramètre, car au départ le servletContext
		// n'est pas forcément défini si c'est un driver jdbc sans dataSource
		return Parameter.DISABLED.getValueAsBoolean();
	}

	Statement createStatementProxy(String query, Statement statement) {
		assert statement != null;
		// Si un proxy de connexion a été créé dans un driver jdbc et que par la suite le
		// servletContext a un paramètre désactivant le monitoring, alors ce n'est pas grave
		// les requêtes sql seront simplement agrégées dans le counter pour cette connexion
		// jusqu'à sa fermeture (pour éviter ce détail, il suffirait simplement d'utiliser une
		// dataSource jdbc et pas un driver).
		// Rq : on ne réévalue pas le paramètre ici pour raison de performances sur la recherche
		// dans les paramètres du système, du contexte et du filtre alors que dans 99.999999999%
		// des exécutions il n'y a pas le paramètre.
		final InvocationHandler invocationHandler = new StatementInvocationHandler(query,
				statement);
		return createProxy(statement, invocationHandler);
	}

	static boolean isEqualsMethod(Object methodName, Object[] args) {
		// == for perf (strings interned: == is ok)
		return "equals" == methodName && args != null && args.length == 1; // NOPMD
	}

	static boolean isHashCodeMethod(Object methodName, Object[] args) {
		// == for perf (strings interned: == is ok)
		return "hashCode" == methodName && (args == null || args.length == 0); // NOPMD
	}

	static <T> T createProxy(T object, InvocationHandler invocationHandler) {
		return createProxy(object, invocationHandler, null);
	}

	static <T> T createProxy(T object, InvocationHandler invocationHandler,
			List<Class<?>> interfaces) {
		if (isProxyAlready(object)) {
			// si l'objet est déjà un proxy créé pas nous, initialisé par exemple
			// depuis SessionListener ou MonitoringInitialContextFactory,
			// alors il ne faut pas faire un proxy du proxy
			return object;
		}
		final InvocationHandler ih = new DelegatingInvocationHandler(invocationHandler);
		return JdbcWrapperHelper.createProxy(object, ih, interfaces);
	}

	private static boolean isProxyAlready(Object object) {
		return Proxy.isProxyClass(object.getClass()) && Proxy.getInvocationHandler(object)
				.getClass().getName().equals(DelegatingInvocationHandler.class.getName());
		// utilisation de Proxy.getInvocationHandler(object).getClass().getName().equals(DelegatingInvocationHandler.class.getName())
		// et non de Proxy.getInvocationHandler(object) instanceof DelegatingInvocationHandler
		// pour issue 97 (classLoaders différents pour les classes DelegatingInvocationHandler)
	}

}
