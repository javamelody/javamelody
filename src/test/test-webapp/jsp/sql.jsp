<%@ page session="false" %>
<%	
	final javax.sql.DataSource ds = (javax.sql.DataSource) new javax.naming.InitialContext()
			.lookup("java:comp/env/jdbc/TestDB");
	final java.sql.Connection connection = ds.getConnection();
	connection.setAutoCommit(false);
	try {
		connection.createStatement().execute("DROP ALIAS if exists SLEEP; CREATE ALIAS SLEEP FOR \"java.lang.Thread.sleep(long)\"");
		// 1 seconde pour avoir une 1ère requête
		connection.createStatement().execute("CALL SLEEP(1000)");
		// 4 secondes pour une 2ème requête,
		// si mysql, on ne peut pas mettre 5s en raison du timeout paramétré sur la datasource
		connection.createStatement().execute("CALL SLEEP(4000)");
	} finally {
		connection.rollback();
		connection.close();
	}
%>

Waited 5s in sql

<br/>
<a href="../index.jsp">back</a>
