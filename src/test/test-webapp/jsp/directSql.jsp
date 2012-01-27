<%@ page session="false" %>
<%	
	final java.util.Properties properties = new java.util.Properties();
	final String query = "DROP ALIAS if exists SLEEP; CREATE ALIAS SLEEP FOR \"java.lang.Thread.sleep(long)\"; call sleep(.1)";
	final String url = "jdbc:h2:/Applis/h2/test-webapp";
	properties.put("user", "sa");
    properties.put("password", "");
    properties.put("driver", "org.h2.Driver");
    
	//final String query = "select sleep(.1)";
	//final String url = "jdbc:mysql://127.0.0.1:3306/test?socketTimeout=5000&amp;connectTimeout=5000";
	//properties.put("user", "root");
    //properties.put("password", "root");
    //properties.put("driver", "com.mysql.jdbc.Driver");

    //final String query = "select 1 from dual";
    //final String url = "jdbc:oracle:thin:@127.0.0.1:1521:";
    //properties.put("user", "test");
    //properties.put("password", "test");
    //properties.put("driver", "oracle.jdbc.OracleDriver");
    
    Class.forName("net.bull.javamelody.JdbcDriver");
	final java.sql.Connection connection = java.sql.DriverManager.getConnection(url, properties);
	connection.setAutoCommit(false);
	try {
		connection.createStatement().execute(query);
	} finally {
		connection.rollback();
		connection.close();
	}
%>

direct sql done

<br/>
<a href="../index.jsp">back</a>
