<%@ page session="false" %>
<%	
	final javax.sql.DataSource ds = (javax.sql.DataSource) new javax.naming.InitialContext()
			.lookup("java:comp/env/jdbc/TestDB");
	final java.sql.Connection connection = ds.getConnection();
	try {
		// 1 seconde pour avoir une 1ère requête
		connection.createStatement().executeQuery("select sleep(1)");
		// 4 secondes pour une 2ème requête,
		// on ne peut pas mettre 5s en raison du timeout paramétré sur la datasource
		connection.createStatement().executeQuery("select \nsleep(4)");
	} finally {
		connection.rollback();
		connection.close();
	}
%>

Waited 5s in sql

<br/>
<a href="../index.jsp">back</a>
