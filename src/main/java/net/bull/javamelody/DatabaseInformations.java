/*
 * Copyright 2008-2012 by Emeric Vernat
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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
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
	private static final long serialVersionUID = -6105478981257689782L;

	static enum Database {
		POSTGRESQL, MYSQL, ORACLE, DB2, H2, HSQLDB;

		// RESOURCE_BUNDLE_BASE_NAME vaut "net.bull.javamelody.resource.databaseInformations"
		// ce qui charge net.bull.javamelody.resource.databaseInformations.properties
		// (Parameters.getResourcePath("databaseInformations") seul ne fonctionne pas si on est dans un jar/war)
		private static final String RESOURCE_BUNDLE_BASE_NAME = Parameters
				.getResourcePath("databaseInformations").replace('/', '.').substring(1);

		List<String> getRequestNames() {
			final List<String> tmp;
			switch (this) {
			case POSTGRESQL:
				tmp = Arrays.asList("pg_stat_activity", "pg_locks", "pg_database", "pg_tablespace",
						"pg_stat_database", "pg_stat_user_tables", "pg_stat_user_indexes",
						"pg_statio_user_tables", "pg_statio_user_indexes",
						"pg_statio_user_sequences", "pg_settings");
				break;
			case MYSQL:
				tmp = Arrays.asList("processlist", "databases", "variables", "global_status",
						"innodb_status");
				break;
			case ORACLE:
				tmp = Arrays
						.asList("sessions", "locks", "sqlTimes", "foreignKeysWithoutIndexes",
								"invalidObjects", "disabledConstraints", "instance", "database",
								"nlsParameters", "tablespaceFreespace", "datafileIo",
								"tablespaceExtents", "ratios", "parameters",
								"rollbackSegmentStatistics", "statistics", "events");
				break;
			case DB2:
				tmp = Arrays.asList("current_queries");
				break;
			case H2:
				tmp = Arrays.asList("memory", "sessions", "locks", "settings");
				break;
			case HSQLDB:
				tmp = Arrays.asList("system_sessions", "system_cacheinfo", "system_properties",
						"system_schemas");
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

	private final Database database;
	@SuppressWarnings("all")
	private final List<String> requestNames;
	private final int selectedRequestIndex;
	private final String[][] result;

	DatabaseInformations(int selectedRequestIndex) throws SQLException, NamingException {
		super();
		this.selectedRequestIndex = selectedRequestIndex;
		final Connection connection = getConnection();
		assert connection != null;
		try {
			database = Database.getDatabaseForConnection(connection);
			requestNames = database.getRequestNames();
			final String request = database
					.getRequestByName(requestNames.get(selectedRequestIndex));
			result = executeRequest(connection, request, null);
		} finally {
			connection.close();
		}
	}

	static int parseRequestIndex(String requestIndex) {
		if (requestIndex != null) {
			return Integer.parseInt(requestIndex);
		}
		return 0;
	}

	int getNbColumns() {
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

	int getSelectedRequestIndex() {
		return selectedRequestIndex;
	}

	String getSelectedRequestName() {
		return requestNames.get(getSelectedRequestIndex());
	}

	String[][] getResult() {
		return result; // NOPMD
	}

	List<String> getRequestNames() {
		return requestNames;
	}

	private static String[][] executeRequest(Connection connection, String request,
			List<?> parametersValues) throws SQLException {
		final PreparedStatement statement = connection.prepareStatement(request);
		try {
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
				throw createGrantException(userName, e);
			}
			throw e;
		} finally {
			statement.close();
		}
	}

	private static String[][] executeQuery(PreparedStatement statement) throws SQLException {
		final ResultSet resultSet = statement.executeQuery();
		try {
			final ResultSetMetaData metaData = resultSet.getMetaData();
			final int columnCount = metaData.getColumnCount();
			final List<String[]> list = new ArrayList<String[]>();
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
			return list.toArray(new String[list.size()][]);
		} finally {
			resultSet.close();
		}
	}

	private static SQLException createGrantException(String userName, SQLException e) {
		final SQLException ex = new SQLException(I18N.getFormattedString(
				"oracle.grantSelectAnyDictionnary", userName));
		ex.initCause(e);
		return ex;
	}

	private static Connection getConnection() throws SQLException, NamingException {
		// on commence par voir si le driver jdbc a été utilisé
		// car s'il n'y a pas de datasource une exception est déclenchée
		if (Parameters.getLastConnectUrl() != null) {
			final Connection connection = DriverManager.getConnection(
					Parameters.getLastConnectUrl(), Parameters.getLastConnectInfo());
			connection.setAutoCommit(false);
			return connection;
		}

		// on cherche une datasource avec InitialContext
		// (le nom de la dataSource recherchée dans JNDI est du genre jdbc/Xxx qui est le nom standard d'une DataSource)
		final Collection<DataSource> dataSources = JdbcWrapper.getJndiAndSpringDataSources()
				.values();
		if (!dataSources.isEmpty()) {
			final Connection connection = dataSources.iterator().next().getConnection();
			connection.setAutoCommit(false);
			return connection;
		}
		return null;
	}

	static String explainPlanFor(String sqlRequest) throws SQLException, NamingException {
		final Connection connection = getConnection();
		if (connection != null) {
			try {
				final Database database = Database.getDatabaseForConnection(connection);
				if (database == Database.ORACLE) {
					// Si oracle, on demande le plan d'exécution avec la table PLAN_TABLE par défaut
					// avec "explain plan set statement_id = <statement_id> for ..."
					// (si mysql ou postgresql on pourrait faire "explain ...",
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
					final String explainRequest = buildExplainRequest(sqlRequest, statementId);
					// exécution de la demande
					final Statement statement = connection.createStatement();
					try {
						statement.execute(explainRequest);
					} finally {
						statement.close();
					}

					// récupération du résultat
					return getPlanOutput(connection, statementId);
				}
			} finally {
				connection.rollback();
				connection.close();
			}
		}
		return null;
	}

	private static String buildExplainRequest(String sqlRequest, String statementId) {
		// rq : il semble qu'une requête explain plan ne puisse avoir la requête en paramètre bindé
		// (donc les requêtes "explain ..." seront ignorées dans JdbcWrapper)
		int i = 1;
		// table PLAN_TABLE par défaut
		// (il faut que cette table soit créée auparavant dans oracle
		// et elle peut être créée par : @$ORACLE_HOME/rdbms/admin/catplan.sql
		// ou par @$ORACLE_HOME/rdbms/admin/utlxplan.sql si oracle 9g ou avant)
		String explainRequest = "explain plan set statement_id = '" + statementId + "' for "
				+ sqlRequest;

		// dans le cas où la requête contient ';' (requêtes multiples), je ne sais pas si explain
		// plan considère que cela fait partie de la requête à analyser où si certaines versions
		// d'oracle considèrent que cela vient après l'explain plan; par sécurité on interdit cela
		if (explainRequest.indexOf(';') != -1) {
			explainRequest = explainRequest.substring(0, explainRequest.indexOf(';'));
		}

		// on remplace les paramètres bindés "?" par ":n"
		int index = explainRequest.indexOf('?');
		while (index != -1) {
			explainRequest = explainRequest.substring(0, index) + ':' + i
					+ explainRequest.substring(index + 1);
			i++;
			index = explainRequest.indexOf('?');
		}
		return explainRequest;
	}

	private static String getPlanOutput(Connection connection, String statementId)
			throws SQLException {
		// table PLAN_TABLE par défaut et format par défaut
		final String planTableRequest = "select * from table(dbms_xplan.display(null,?, null))";
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
	}
}
