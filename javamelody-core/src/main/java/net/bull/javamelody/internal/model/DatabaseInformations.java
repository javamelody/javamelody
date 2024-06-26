/*
 * Copyright 2008-2019 by Emeric Vernat
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
package net.bull.javamelody.internal.model;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.naming.NamingException;
import javax.sql.DataSource;

import org.postgresql.jdbc.PgConnection;
import org.postgresql.jdbc.PreferQueryMode;

import net.bull.javamelody.JdbcWrapper;
import net.bull.javamelody.Parameter;
import net.bull.javamelody.internal.common.I18N;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Informations sur la base de données.
 * @author Emeric Vernat
 */
public class DatabaseInformations implements Serializable {
	private static final long serialVersionUID = -6105478981257689782L;
	private static final boolean POSTGRESQL_DRIVER_AVAILABLE = isPostgresqlDriverAvailable();

	enum Database {
		// base de données connues avec les noms retournés par connection.getMetaData().getDatabaseProductName()
		// (inspirés par Hibernate)
		POSTGRESQL("PostgreSQL"),
		MYSQL("MySQL"),
		MARIADB("MariaDB"),
		ORACLE("Oracle"),
		DB2("DB2 UDB for AS/400", "DB2/"),
		H2("H2"),
		HSQLDB("HSQL Database Engine"),
		SQLSERVER("Microsoft SQL Server"),
		SYBASE("Sybase SQL Server", "Adaptive Server Enterprise"),
		INFORMIX("Informix Dynamic Server"),
		SQLITE("SQLite");

		// RESOURCE_BUNDLE_BASE_NAME vaut "net.bull.javamelody.resource.databaseInformations"
		// ce qui charge net.bull.javamelody.resource.databaseInformations.properties
		// (Parameters.getResourcePath("databaseInformations") seul ne fonctionne pas si on est dans un jar/war)
		private static final String RESOURCE_BUNDLE_BASE_NAME = Parameters
				.getResourcePath("databaseInformations").replace('/', '.').substring(1);

		private final List<String> databaseNames;

		Database(String... databaseNames) {
			this.databaseNames = List.of(databaseNames);
		}

		// CHECKSTYLE:OFF
		List<String> getRequestNames() {
			// CHECKSTYLE:ON
			final List<String> tmp;
			switch (this) {
			case POSTGRESQL:
				tmp = List.of("pg_stat_activity", "pg_locks", "pg_database", "pg_tablespace",
						"pg_stat_database", "pg_stat_user_tables", "pg_stat_user_indexes",
						"pg_statio_user_tables", "pg_statio_user_indexes",
						"pg_statio_user_sequences", "pg_settings");
				break;
			case MYSQL:
			case MARIADB:
				tmp = List.of("processlist", "databases", "variables", "global_status",
						"innodb_status", "unusedIndexes", "longRunning", "tableStats",
						"eventsWaits", "tableIoWaits", "indexIoWaits", "tableLockWaits",
						"tablesWithoutPk", "perfDigests", "memory");
				break;
			case ORACLE:
				tmp = List.of("sessions", "locks", "sqlTimes", "foreignKeysWithoutIndexes",
						"invalidObjects", "disabledConstraints", "tableStats", "instance",
						"database", "nlsParameters", "tablespaceFreespace", "datafileIo",
						"tablespaceExtents", "ratios", "parameters", "rollbackSegmentStatistics",
						"statistics", "events");
				break;
			case DB2:
				tmp = List.of("mon_current_sql", "mon_db_summary", "mon_lockwaits",
						"mon_service_subclass_summary", "mon_current_uow", "mon_workload_summary",
						"mon_get_connection", "current_queries");
				break;
			case H2:
				tmp = List.of("memory", "sessions", "locks", "settings");
				break;
			case HSQLDB:
				tmp = List.of("system_sessions", "system_cacheinfo", "system_properties",
						"system_schemas");
				break;
			case SQLSERVER:
				tmp = List.of("version", "connections");
				break;
			case SYBASE:
				tmp = List.of("sp_who", "connections", "sp_lock", "lock",
						"running_stored_procedure", "used_temporary_tables", "used_tables",
						"sp_version");
				break;
			case INFORMIX:
				tmp = List.of("version", "sessions", "resources_by_user", "current_queries",
						"config");
				break;
			case SQLITE:
				tmp = List.of("version", "database_list");
				break;
			default:
				throw new IllegalStateException();
			}
			return addPrefix(tmp);
		}

		private List<String> addPrefix(List<String> requests) {
			final List<String> list = new ArrayList<>(requests.size());
			final String prefix = this.toString().toLowerCase(Locale.ENGLISH) + '.';
			for (final String requestName : requests) {
				list.add(prefix + requestName);
			}
			return list;
		}

		String getUrlIdentifier() {
			return this.toString().toLowerCase(Locale.ENGLISH);
		}

		String getRequestByName(String requestName) {
			return ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME).getString(requestName);
		}

		List<String> getDatabaseNames() {
			return databaseNames;
		}

		private boolean isRecognized(String databaseName, String url) {
			for (final String name : getDatabaseNames()) {
				if (databaseName.startsWith(name)) {
					return true;
				}
			}
			return url != null && url.contains(getUrlIdentifier());
		}

		static Database getDatabaseForConnection(Connection connection) throws SQLException {
			final DatabaseMetaData metaData = connection.getMetaData();
			final String databaseName = metaData.getDatabaseProductName();
			final String url = metaData.getURL();
			for (final Database database : values()) {
				if (database.isRecognized(databaseName, url)) {
					return database;
				}
			}
			throw new IllegalArgumentException(
					I18N.getFormattedString("type_base_de_donnees_inconnu", databaseName));
		}
	}

	private final Database database;
	@SuppressWarnings("all")
	private final List<String> requestNames;
	private final int selectedRequestIndex;
	private final String[][] result;

	public DatabaseInformations(int selectedRequestIndex) throws SQLException, NamingException {
		super();
		this.selectedRequestIndex = selectedRequestIndex;
		try (final Connection connection = getConnection()) {
			assert connection != null;
			database = Database.getDatabaseForConnection(connection);
			requestNames = database.getRequestNames();
			final String request = database
					.getRequestByName(requestNames.get(selectedRequestIndex));
			result = executeRequest(connection, request, null);
		}
	}

	public static int parseRequestIndex(String requestIndex) {
		if (requestIndex != null) {
			return Integer.parseInt(requestIndex);
		}
		return 0;
	}

	public int getNbColumns() {
		final String selectedRequestName = getSelectedRequestName();
		if ("oracle.statistics".equals(selectedRequestName)) {
			return 2;
		} else if ("oracle.events".equals(selectedRequestName)) {
			return 2;
		} else if ("mysql.variables".equals(selectedRequestName)) {
			return 2;
		} else if ("mysql.global_status".equals(selectedRequestName)) {
			return 4;
		} else if ("h2.settings".equals(selectedRequestName)) {
			return 2;
		}
		return 1;
	}

	public int getSelectedRequestIndex() {
		return selectedRequestIndex;
	}

	public String getSelectedRequestName() {
		return requestNames.get(getSelectedRequestIndex());
	}

	public String[][] getResult() {
		return result; // NOPMD
	}

	public List<String> getRequestNames() {
		return requestNames;
	}

	private static String[][] executeRequest(Connection connection, String request,
			List<?> parametersValues) throws SQLException {
		try (final PreparedStatement statement = connection.prepareStatement(request)) {
			if (parametersValues != null) {
				int i = 1;
				for (final Object parameterValue : parametersValues) {
					statement.setObject(i, parameterValue);
					i++;
				}
			}
			return executeQuery(statement);
		} catch (final SQLException e) {
			if (e.getErrorCode() == 942 && e.getMessage() != null
					&& e.getMessage().startsWith("ORA-")) {
				final String userName = connection.getMetaData().getUserName();
				final String message = I18N.getFormattedString("oracle.grantSelectAnyDictionnary",
						userName);
				throw new SQLException(message, e);
			}
			throw e;
		}
	}

	private static String[][] executeQuery(PreparedStatement statement) throws SQLException {
		try (final ResultSet resultSet = statement.executeQuery()) {
			final ResultSetMetaData metaData = resultSet.getMetaData();
			final int columnCount = metaData.getColumnCount();
			final List<String[]> list = new ArrayList<>();
			String[] values = new String[columnCount];
			for (int i = 1; i <= columnCount; i++) {
				values[i - 1] = metaData.getColumnName(i) + '\n' + metaData.getColumnTypeName(i)
						+ '(' + metaData.getColumnDisplaySize(i) + ')';
			}
			list.add(values);

			while (resultSet.next()) {
				values = new String[columnCount];
				for (int i = 1; i <= columnCount; i++) {
					values[i - 1] = resultSet.getString(i);
				}
				list.add(values);
			}
			return list.toArray(new String[0][]);
		}
	}

	private static Connection getConnection() throws SQLException, NamingException {
		// on commence par voir si le driver jdbc a été utilisé
		// car s'il n'y a pas de datasource une exception est déclenchée
		if (Parameters.getLastConnectUrl() != null) {
			final Connection connection = DriverManager
					.getConnection(Parameters.getLastConnectUrl(), Parameters.getLastConnectInfo());
			connection.setAutoCommit(false);
			return connection;
		}

		// on cherche une datasource avec InitialContext
		// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
		final Collection<DataSource> dataSources = JdbcWrapper.getJndiAndSpringDataSources()
				.values();
		for (final DataSource dataSource : dataSources) {
			try {
				return dataSource.getConnection();
				// on ne doit pas changer autoCommit pour la connection d'une DataSource
				// (ou alors il faudrait remettre l'autoCommit après, issue 189)
				// connection.setAutoCommit(false);
			} catch (final Exception e) {
				// si cette dataSource ne fonctionne pas, on suppose que la bonne dataSource est une des suivantes
				// (par exemple, sur GlassFish il y a des dataSources par défaut qui ne fonctionne pas forcément)
				continue;
			}
		}
		if (!dataSources.isEmpty()) {
			// this will probably throw an exception like above
			return dataSources.iterator().next().getConnection();
		}
		return null;
	}

	public static String explainPlanFor(String sqlRequest) throws SQLException, NamingException {
		final Connection connection = getConnection();
		if (connection != null) {
			try {
				final Database database = Database.getDatabaseForConnection(connection);
				if (database == Database.ORACLE) {
					// Si oracle, on demande le plan d'exécution avec la table PLAN_TABLE par défaut
					// avec "explain plan set statement_id = <statement_id> for ..."
					// (si mysql, on pourrait faire "explain ...",
					// sauf que les paramètres bindés ne seraient pas acceptés
					// et les requêtes update/insert/delete non plus).
					// (si db2, la syntaxe serait "explain plan for ...")

					// Si mysql il suffit de lire le ResultSet de executeQuery("explain ...")
					// qui pourrait être affiché en tableau à partir de String[][],
					// mais en oracle il faut aller lire la table plan_table
					// (http://www.java2s.com/Open-Source/Java-Document/Database-Client/squirrel-sql-2.6.5a/net/sourceforge/squirrel_sql/plugins/oracle/explainplan/ExplainPlanExecuter.java.htm)
					// le hashCode est une clé suffisamment unique car il y a peu de plans d'exécution
					// affichés simultanément, et en tout cas CounterRequest.getId() est trop long
					// pour la table oracle par défaut (SYS.PLAN_TABLE$.STATEMENT_ID a une longueur de 30)
					final String statementId = String.valueOf(sqlRequest.hashCode());
					// utilisation de la table PLAN_TABLE par défaut
					// (il faut que cette table soit créée auparavant dans oracle
					// et elle peut être créée par : @$ORACLE_HOME/rdbms/admin/catplan.sql
					// ou par @$ORACLE_HOME/rdbms/admin/utlxplan.sql si oracle 9g ou avant)
					final String explainRequest = "explain plan set statement_id = '" + statementId
							+ "' for " + normalizeRequestForExplain(sqlRequest, ':');
					// exécution de la demande
					try (final Statement statement = connection.createStatement()) {
						statement.execute(explainRequest);
					}

					// récupération du résultat
					// table PLAN_TABLE par défaut et format par défaut
					final String planTableRequest = "select * from table(dbms_xplan.display(null,?,null))";
					final String[][] planTableOutput = executeRequest(connection, planTableRequest,
							Collections.singletonList(statementId));
					final StringBuilder sb = new StringBuilder();
					for (final String[] row : planTableOutput) {
						for (final String value : row) {
							sb.append(value);
						}
						sb.append('\n');
					}
					if (sb.indexOf("-") != -1) {
						sb.delete(0, sb.indexOf("-"));
					}
					return sb.toString();
				} else if (database == Database.POSTGRESQL && POSTGRESQL_DRIVER_AVAILABLE
						&& connection.getMetaData().getDatabaseMajorVersion() >= 16) {
					// Si postgresql, on demande le plan d'exécution avec "explain (generic plan) ..."
					final PgConnection pgConnection = connection.unwrap(PgConnection.class);
					if (pgConnection != null) {
						final PreferQueryMode preferQueryMode = pgConnection.getQueryExecutor()
								.getPreferQueryMode();
						try {
							// given the parameters without values, explain (generic plan) should be executed as simple query
							// and not as prepared query which is by default.
							// (there is no other way than using postgresql "internal" api:
							// not possible using jdbc api or postgresql "public" api)
							pgConnection.getQueryExecutor()
									.setPreferQueryMode(PreferQueryMode.SIMPLE);

							// explain plan pour Postgresql 16 ou ultérieur
							// https://www.cybertec-postgresql.com/en/explain-generic-plan-postgresql-16/
							final String explainPlanRequest = "explain (generic_plan) "
									+ normalizeRequestForExplain(sqlRequest, '$');
							final StringBuilder sb = new StringBuilder();
							try (final Statement statement = connection.createStatement()) {
								try (final ResultSet resultSet = statement
										.executeQuery(explainPlanRequest)) {
									while (resultSet.next()) {
										sb.append(resultSet.getString(1)).append('\n');
									}
								}
							}
							return sb.toString();
						} finally {
							// set back the connection preferQueryMode as before
							pgConnection.getQueryExecutor().setPreferQueryMode(preferQueryMode);
						}
					}
				}
			} finally {
				if (!connection.getAutoCommit()) {
					connection.rollback();
				}
				connection.close();
			}
		}
		return null;
	}

	private static String normalizeRequestForExplain(String sqlRequest, char parameterChar) {
		// rq : il semble qu'une requête explain plan ne puisse avoir la requête en paramètre bindé
		// (donc les requêtes "explain ..." seront ignorées dans JdbcWrapper)
		int i = 1;
		String request = sqlRequest;
		if (Parameter.SQL_TRANSFORM_PATTERN.getValue() != null) {
			// si les requêtes SQL peuvent avoir été transformées par SQL_TRANSFORM_PATTERN,
			// alors on remplace le '$' par '?' en espérant avec un plan d'exécution même simplifié
			// (sinon, il serait impossible d'avoir un plan d'exécution pour certaines requêtes SQL
			// transformées par SQL_TRANSFORM_PATTERN)
			request = request.replace(Counter.TRANSFORM_REPLACEMENT_CHAR, '?');
		}

		// dans le cas où la requête contient ';' (requêtes multiples), je ne sais pas si explain
		// plan considère que cela fait partie de la requête à analyser où si certaines versions
		// d'oracle considèrent que cela vient après l'explain plan; par sécurité on interdit cela
		if (request.indexOf(';') != -1) {
			request = request.substring(0, request.indexOf(';'));
		}

		// on remplace les paramètres bindés "?" par ":n" si oracle ou "$n" si postgresql
		int index = request.indexOf('?');
		while (index != -1) {
			request = request.substring(0, index) + parameterChar + i
					+ request.substring(index + 1);
			i++;
			index = request.indexOf('?');
		}
		return request;
	}

	private static boolean isPostgresqlDriverAvailable() {
		try {
			Class.forName("org.postgresql.jdbc.PgConnection");
			return true;
		} catch (final ClassNotFoundException e) {
			return false;
		}
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[database=" + database + ']';
	}
}
