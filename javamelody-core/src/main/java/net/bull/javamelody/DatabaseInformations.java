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
		POSTGRESQL, MYSQL, ORACLE;

		List<String> getRequestNames() {
			final List<String> tmp;
			switch (this) {
			case POSTGRESQL:
				tmp = Arrays.asList("pg_stat_activity", "pg_stat_database", "pg_stat_user_tables",
						"pg_stat_user_indexes", "pg_statio_user_tables", "pg_statio_user_indexes",
						"pg_statio_user_sequences", "pg_settings");
				break;
			case MYSQL:
				tmp = Arrays.asList("processlist", "databases", "status", "innodb_status");
				break;
			case ORACLE:
				tmp = Arrays
						.asList("sessions", "locks", "sqlTimes", "instance", "database",
								"nlsParameters", "tablespaceFreespace", "datafileIo",
								"tablespaceExtents", "ratios", "parameters",
								"rollbackSegmentStatistics", "statistics", "events");
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
			if (url.contains("postgres")) {
				return POSTGRESQL;
			} else if (url.contains("mysql")) {
				return MYSQL;
			} else if (url.contains("oracle")) {
				return ORACLE;
			}
			throw new IllegalArgumentException("Type de connexion inconnu: " + url);
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

	private static Connection getConnection() throws Exception {
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
}
