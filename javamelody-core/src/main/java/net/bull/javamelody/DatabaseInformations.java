/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.io.Serializable;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.naming.NamingException;
import javax.sql.DataSource;

/**
 * Informations sur la base de données.
 * @author Emeric Vernat
 */
class DatabaseInformations implements Serializable {
	// RESOURCE_BUNDLE_BASE_NAME vaut "net.bull.javamelody.resource.databaseInformations"
	// ce qui charge net.bull.javamelody.resource.databaseInformations.properties
	// (Parameters.getResourcePath("databaseInformations") seul ne fonctionne pas si on est dans un jar/war)
	static final String RESOURCE_BUNDLE_BASE_NAME = Parameters.getResourcePath(
			"databaseInformations").replace('/', '.').substring(1);
	private static final long serialVersionUID = -6105478981257689782L;

	private static enum Database {
		POSTGRESQL, MYSQL, ORACLE, DB2, H2;

		List<String> getRequestNames() {
			final List<String> tmp;
			switch (this) {
			case POSTGRESQL:
				tmp = Arrays.asList("pg_stat_activity", "pg_stat_database", "pg_stat_user_tables",
						"pg_stat_user_indexes", "pg_statio_user_tables", "pg_statio_user_indexes",
						"pg_statio_user_sequences", "pg_settings");
				break;
			case MYSQL:
				tmp = Arrays.asList("processlist", "databases", "variables", "global_status",
						"innodb_status");
				break;
			case ORACLE:
				tmp = Arrays
						.asList("sessions", "locks", "sqlTimes", "instance", "database",
								"nlsParameters", "tablespaceFreespace", "datafileIo",
								"tablespaceExtents", "ratios", "parameters",
								"rollbackSegmentStatistics", "statistics", "events");
				break;
			case DB2:
				tmp = Arrays.asList("current_queries");
				break;
			case H2:
				tmp = Arrays.asList("memory");
				break;
			default:
				throw new IllegalStateException();
			}
			final List<String> result = new ArrayList<String>(tmp.size());
			final String prefix = this.toString().toLowerCase(Locale.getDefault()) + '.';
			for (final String requestName : tmp) {
				result.add(prefix + requestName);
			}
			return result;
		}

		String getRequestByName(String requestName) {
			return ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME).getString(requestName);
		}

		static Database getDatabaseForConnection(Connection connection) throws SQLException {
			final String url = connection.getMetaData().getURL();
			for (final Database database : Database.values()) {
				if (url.contains(database.toString().toLowerCase(Locale.getDefault()))) {
					return database;
				}
			}
			throw new IllegalArgumentException(I18N.getFormattedString(
					"type_base_de_donnees_inconnu", url));
		}
	}

	private final int requestIndex;
	@SuppressWarnings("all")
	private final List<String> requestNames;
	private final String[][] result;

	DatabaseInformations(int requestIndex) throws Exception {
		super();
		this.requestIndex = requestIndex;
		final Connection connection = getConnection();
		try {
			final Database database = Database.getDatabaseForConnection(connection);
			requestNames = database.getRequestNames();
			final String request = database.getRequestByName(requestNames.get(requestIndex));
			result = executeRequest(connection, request);
		} finally {
			connection.close();
		}
	}

	int getRequestIndex() {
		return requestIndex;
	}

	String[][] getResult() {
		return result;
	}

	List<String> getRequestNames() {
		return requestNames;
	}

	private static String[][] executeRequest(Connection connection, String request)
			throws SQLException {
		final Statement statement = connection.createStatement();
		try {
			final ResultSet resultSet = statement.executeQuery(request);
			try {
				final ResultSetMetaData metaData = resultSet.getMetaData();
				final int columnCount = metaData.getColumnCount();
				final List<String[]> list = new ArrayList<String[]>();
				String[] values = new String[columnCount];
				for (int i = 1; i <= columnCount; i++) {
					values[i - 1] = metaData.getColumnName(i) + '\n'
							+ metaData.getColumnTypeName(i) + '('
							+ metaData.getColumnDisplaySize(i) + ')';
				}
				list.add(values);

				while (resultSet.next()) {
					values = new String[columnCount];
					for (int i = 1; i <= columnCount; i++) {
						values[i - 1] = resultSet.getString(i);
					}
					list.add(values);
				}
				return list.toArray(new String[list.size()][]);
			} finally {
				resultSet.close();
			}
		} catch (final SQLException e) {
			if (e.getErrorCode() == 942 && e.getMessage() != null
					&& e.getMessage().startsWith("ORA-")) {
				final SQLException ex = new SQLException(I18N.getFormattedString(
						"oracle.grantSelectAnyDictionnary", connection.getMetaData().getUserName()));
				ex.initCause(e);
				throw ex;
			}
			throw e;
		} finally {
			statement.close();
		}
	}

	private static Connection getConnection() throws SQLException, NamingException {
		// on commence par voir si le driver jdbc a été utilisé
		// car s'il n'y a pas de datasource une exception est déclenchée
		final JdbcDriver jdbcDriver = JdbcDriver.SINGLETON;
		if (jdbcDriver.getLastConnectUrl() != null) {
			final Connection connection = DriverManager.getConnection(jdbcDriver
					.getLastConnectUrl(), jdbcDriver.getLastConnectInfo());
			connection.setAutoCommit(false);
			return connection;
		}

		// on cherche une datasource avec InitialContext
		// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
		final Collection<DataSource> dataSources = JdbcWrapperHelper.getDataSources().values();
		if (!dataSources.isEmpty()) {
			final Connection connection = dataSources.iterator().next().getConnection();
			connection.setAutoCommit(false);
			return connection;
		}
		return null;
	}

	// TODO explain plan
	//	static String[][] explainPlanFor(String request) throws SQLException, NamingException {
	//		final Connection connection = getConnection();
	//		if (connection != null) {
	//			final Database database = Database.getDatabaseForConnection(connection);
	//			if (database == Database.ORACLE || database == Database.DB2) {
	//				// Si oracle ou db2, on demande le plan d'exécution (explain plan)
	//				// par "explain plan for ..."
	//				// (si mysql ou postgresql on pourrait faire "explain ...",
	//				// sauf que les paramètres bindés ne seraient pas acceptés
	//				// et les requêtes update/insert/delete non plus).
	//
	//				// Si mysql il suffit de lire le ResultSet de executeQuery("explain ...")
	//				// mais en oracle il faut aller lire la table plan_table ou autre
	//				// (http://www.java2s.com/Open-Source/Java-Document/Database-Client/squirrel-sql-2.6.5a/net/sourceforge/squirrel_sql/plugins/oracle/explainplan/ExplainPlanExecuter.java.htm)
	//				try {
	//					int i = 1;
	//					String explainRequest = "explain plan for " + request;
	//					// on remplace les paramètres bindés "?" par ":n"
	//					int index = explainRequest.indexOf('?');
	//					while (index != -1) {
	//						explainRequest = explainRequest.substring(0, index) + ':' + i
	//								+ explainRequest.substring(index + 1);
	//						i++;
	//						index = explainRequest.indexOf('?');
	//					}
	//					// exécution de la demande (sans types des colonnes)
	//					return executeRequest(connection, explainRequest, false);
	//				} finally {
	//					connection.rollback();
	//					connection.close();
	//				}
	//			}
	//		}
	//		return null;
	//	}
}
